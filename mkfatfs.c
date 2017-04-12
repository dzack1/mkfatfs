/* KallistiOS ##version##

   mkfatfs.c
   Copyright (C) 2017 Lawrence Sebald

   This example shows how to format a volume with a FAT filesystem. This tool
   will automatically pick the appropriate format (FAT16 or FAT32) based on the
   volume size, and tune the filesystem appropriately too. Volumes less than
   512 MiB in size will be formatted as FAT16, whereas those at least 512 MiB
   in size will be formatted FAT32.

   The cluster size determination algorithm for FAT16 could use a bit of work,
   but it does work at the moment and prioritizes smaller clusters, at the
   expense of a larger FAT. For FAT32, we follow what dosfstools claims is the
   Microsoft standard for cluster size based on the volume size.

   To those in CMSC 421 looking at this code, yes it is adapted from something
   that I work on in my free time. Those of you in my section have at least
   seen me reference it (KallistiOS) a few times in class. You all can safely
   ignore all the _arch_dreamcast stuff. ;-)

*/

#include <time.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>
#include <unistd.h>

#ifdef _arch_dreamcast
#include <kos/dbgio.h>
#include <kos/blockdev.h>

#include <arch/arch.h>

#include <dc/sd.h>
#include <dc/maple.h>
#include <dc/maple/controller.h>
#endif

#include "fatstructs.h"

#define KiB 1024LLU
#define MiB (KiB * 1024LLU)
#define GiB (MiB * 1024LLU)

/* The default number of root directory entries for FAT16. */
#define ROOTDIR_ENTRIES     512

/* Directory entries are always 32 bytes in size. */
#define DIRENT_SIZE         32

/* Assume we'll always have 512 byte physical blocks. */
#define DISKBLOCK_SIZE      512

/* Cutoffs of maximum numbers of clusters per FS type. */
#define FAT12_CUTOFF        4085
#define FAT16_CUTOFF        65525
#define FAT32_CUTOFF        0x0FFFFFF0

/* Filesystem types... Returned by the fat_fs_type function. */
#define FAT12       1
#define FAT16       2
#define FAT32       3

const char *fs_names[3] = {
    "FAT12", "FAT16", "FAT32"
};

/* For use outside of KallistiOS... */
#if !defined(__FAT_FATFS_H) && !defined(_arch_dreamcast)
typedef struct kos_blockdev {
    void *dev_data;
    uint32_t l_block_size;
    int (*init)(struct kos_blockdev *d);
    int (*shutdown)(struct kos_blockdev *d);
    int (*read_blocks)(struct kos_blockdev *d, uint32_t block, size_t count,
                       void *buf);
    int (*write_blocks)(struct kos_blockdev *d, uint32_t block, size_t count,
                        const void *buf);
    uint32_t (*count_blocks)(struct kos_blockdev *d);
} kos_blockdev_t;
#endif

/* Scratch space for storing a single disk block. Aligned for DMA purposes on
   targets where that matters. */
static uint8_t block[DISKBLOCK_SIZE] __attribute__((aligned(32)));

/* Forward declaration. */
static void __attribute__((__noreturn__)) exit_with_error(const char *err);

/* Round up the size of a FAT to an even cluster boundary. */
static uint32_t roundup_fat(int bpc, uint32_t bpf) {
    if(bpf & (bpc - 1)) {
        bpf += bpc;
        bpf &= ~(bpc - 1);
    }

    return bpf;
}

static int format_fat16(kos_blockdev_t *bd, uint32_t block_count,
                        uint64_t partition_size) {
    fat16_boot_t bs;
    time_t now = time(NULL);
    uint32_t blocks_per_cluster = 1;
    uint32_t reserved_blocks;
    uint32_t data_blocks, clusters;
    uint32_t blocks_per_fat;
    uint32_t i;

    (void)partition_size;

    /* Figure out how many blocks of the volume will be reserved by the root
       directory and the BPB. TODO: This should be made adjustable, but the spec
       pretty much says that you should always use 512... */
    reserved_blocks = ((ROOTDIR_ENTRIES * DIRENT_SIZE) / DISKBLOCK_SIZE) + 1;

    /* Make an initial guess at the number of data blocks. This guess will
       obviously be wrong, as we're not including the FAT. */
    data_blocks = block_count - reserved_blocks;

    /* Calculate how many blocks the FAT would take up for one block per
       cluster (since that's what we've got set up at the moment). */
    blocks_per_fat = (data_blocks + 255) / 256;

    /* Now, we know the real block count in the data area... Note that we may
       overestimate the real size of the FAT in this calculation. That's fine,
       as we really need to avoid underestimating it. */
    clusters = data_blocks - (blocks_per_fat * 2);

    /* Bail out early if we know there's no chance this will work right. */
    if(clusters < FAT12_CUTOFF)
        exit_with_error("Volume too small to format as FAT16.");

    /* Now, figure out the smallest cluster size we can use and still accomodate
       the number of blocks we have in our cluster count. */
    while(clusters > FAT16_CUTOFF && blocks_per_cluster <= 128) {
        /* If we've got too many clusters at this point, double the number of
           blocks per cluster. Not exactly the most efficient way to do this,
           but it works. */
        blocks_per_cluster <<= 1;

        /* Recalculate the FAT size, the number of data blocks, and the number
           of clusters. This could be better, but this should be pretty clear as
           it is. */
        clusters = data_blocks / blocks_per_cluster;

        /* If the number of clusters is evenly divisible by the amount of
           clusters per FAT block (256), then we can just do integer division to
           figure out how many blocks we need. Otherwise, we need to add one to
           the result of the (rounded down) integer division to ensure we don't
           run out of space in the FAT. The easiest way to do this is to simply
           add 255 to the number of clusters and then do the integer division,
           saving us a branch (if it is evenly divisible, this won't add 1 to
           the final result, otherwise it will). */
        blocks_per_fat = (clusters + 255) / 256;

        clusters = (data_blocks - (blocks_per_fat * 2)) / blocks_per_cluster;
    }

    /* Calculate the real number of data blocks... */
    data_blocks = block_count - reserved_blocks - (2 * blocks_per_fat);

    /* Did we fail to get a match? */
    if(blocks_per_cluster > 128)
        exit_with_error("Cowardly refusing to make a large FAT16 volume.");

    printf("FAT16 filesystem parameters:\n");
    printf("Cluster size (in blocks): %" PRIu32 " (%" PRIu32 " bytes)\n",
           blocks_per_cluster, blocks_per_cluster * DISKBLOCK_SIZE);
    printf("Number of raw blocks: %" PRIu32 "\n", block_count);
    printf("Data blocks on volume: %" PRIu32 "\n", data_blocks);
    printf("Number of clusters: %" PRIu32 "\n", clusters);
    printf("Size of each FAT (in blocks): %" PRIu32 "\n", blocks_per_fat);
    printf("Number of root directory entries: %d\n", ROOTDIR_ENTRIES);

    /* Fill in the BPB... */
    bs.boot_jmp[0] = 0xEB;              /* Short JMP */
    bs.boot_jmp[1] = 0x3C;              /* To after the BPB. */
    bs.boot_jmp[2] = 0x90;              /* NOP */
    memcpy(bs.oem_id, "KOSmkfat", 8);
    bs.bytes_per_sector[0] = 0x00;      /* 512 byte sectors. */
    bs.bytes_per_sector[1] = 0x02;
    bs.sectors_per_cluster = blocks_per_cluster;
    bs.reserved_sectors = 1;
    bs.fat_copies = 2;
    bs.rootdir_entries[0] = ROOTDIR_ENTRIES & 0xFF;
    bs.rootdir_entries[1] = (ROOTDIR_ENTRIES >> 8) & 0xFF;
    bs.num_sectors_sm[0] = 0;           /* We'll use the 4-byte version later */
    bs.num_sectors_sm[1] = 0;
    bs.media_code = 0xF8;               /* 0xF8 = Not a floppy disk */
    bs.sectors_per_fat = blocks_per_fat;
    bs.sectors_per_track = 1;           /* We don't care about CHS addressing */
    bs.heads = 1;                       /* Ditto. */
    bs.hidden_sectors = 0;
    bs.num_sectors_lg = block_count;
    bs.drive_number = 0x80;             /* Fixed disk */
    bs.chkdsk_needed = 0;
    bs.ext_sig = 0x29;                  /* Magic value, required by FAT */
    bs.serial[0] = now & 0xFF;          /* Set the serial number to the time */
    bs.serial[1] = (now >> 8) & 0xFF;
    bs.serial[2] = (now >> 16) & 0xFF;
    bs.serial[3] = (now >> 24) & 0xFF;
    memcpy(bs.vol_name, "NO NAME    ", 11);
    memcpy(bs.fs_type, "FAT16   ", 8);
    bs.boot_code[0] = 0xCD;             /* INT 18h */
    bs.boot_code[1] = 0x18;
    bs.boot_code[2] = 0xEB;             /* JMP -2 (an infinite loop) */
    bs.boot_code[3] = 0xFE;
    memset(&bs.boot_code[4], 0, 444);
    bs.block_sig[0] = 0x55;             /* Magic value, required by FAT */
    bs.block_sig[1] = 0xAA;

    /* TODO: It'd be nice to align the data area on a nice cluster boundary (or
       at least on a 4KiB boundary), but that isn't strictly necessary. */

    /* Now, all that is left is actually writing data out to the disk... */
    memcpy(block, &bs, 512);
    bd->write_blocks(bd, 0, 1, block);  /* Block 0 = BPB. */

    /* Zero out the block, so we can write it over and over again to fill in the
       root directory. */
    memset(block, 0, 512);
    for(i = 1; i < reserved_blocks; ++i) {
        bd->write_blocks(bd, i, 1, block);
    }

    /* Now, fill in the first two entries of the FAT so we can write the two
       copies of the FAT to the disk. */
    block[0] = 0xF8;
    block[1] = 0xFF;
    block[2] = 0xFF;
    block[3] = 0xFF;
    bd->write_blocks(bd, reserved_blocks, 1, block);
    bd->write_blocks(bd, reserved_blocks + blocks_per_fat, 1, block);

    /* Clear the two entries and fill in the rest of the FATs. */
    block[0] = block[1] = block[2] = block[3] = 0;
    for(i = 1; i < blocks_per_fat; ++i) {
        bd->write_blocks(bd, reserved_blocks + i, 1, block);
        bd->write_blocks(bd, reserved_blocks + i + blocks_per_fat, 1, block);
    }

    /* And, we're done. */
    return 0;
}

static int format_fat32(kos_blockdev_t *bd, uint32_t block_count,
                        uint64_t partition_size) {
    fat32_boot_t bs;
    fat32_info_t is;
    time_t now = time(NULL);
    uint32_t blocks_per_cluster = 1;
    uint32_t reserved_blocks;
    uint32_t data_blocks, clusters;
    uint32_t blocks_per_fat;
    uint64_t vol_size;
    uint32_t i;

    (void)partition_size;

    /* FAT32 always has at least two blocks reserved, but usually has 32. */
    reserved_blocks = 32;

    /* According to dosfstools, Microsoft suggests the following number of
       blocks per cluster, depending on volume size:
         <= 260MiB : 1 block per cluster
         <= 8GiB   : 8 blocks per cluster
         <= 16GiB  : 16 blocks per cluster
         <= 32GiB  : 32 blocks per cluster
         >  32GiB  : 64 blocks per cluster
       We'll mostly follow that suggestion here, with a few extensions.
       Note: This table makes things much cleaner than the FAT16 case. */
    vol_size = block_count * DISKBLOCK_SIZE;
    if(vol_size >= 128 * GiB)
        blocks_per_cluster = 256;
    else if(vol_size >= 64 * GiB)
        blocks_per_cluster = 128;
    else if(vol_size >= 32 * GiB)
        blocks_per_cluster = 64;
    else if(vol_size >= 16 * GiB)
        blocks_per_cluster = 32;
    else if(vol_size >= 8 * GiB)
        blocks_per_cluster = 16;
    else if(vol_size >= 260 * MiB)
        blocks_per_cluster = 8;
    else
        blocks_per_cluster = 1;

    /* Now that we know the number of blocks per cluster, we can calculate the
       size of the FAT... Once again, this might overestimate a bit, but that's
       fine. */
    data_blocks = block_count - 32;
    clusters = data_blocks / blocks_per_cluster;

    /* If the number of clusters is evenly divisible by the amount of
       clusters per FAT block (128), then we can just do integer division to
       figure out how many blocks we need. Otherwise, we need to add one to
       the result of the (rounded down) integer division to ensure we don't
       run out of space in the FAT. The easiest way to do this is to simply
       add 127 to the number of clusters and then do the integer division,
       saving us a branch (if it is evenly divisible, this won't add 1 to
       the final result, otherwise it will). */
    blocks_per_fat = roundup_fat(blocks_per_cluster, (clusters + 127) / 128);

    data_blocks -= (2 * blocks_per_fat);
    clusters = data_blocks / blocks_per_cluster;

    /* Bail out early if we know there's no chance this will work right. */
    if(clusters < FAT16_CUTOFF)
        exit_with_error("Volume too small to format as FAT32.");

    if(clusters > FAT32_CUTOFF)
        exit_with_error("Volume too large to format as FAT32.");

    printf("FAT32 filesystem parameters:\n");
    printf("Cluster size (in blocks): %" PRIu32 " (%" PRIu32 " bytes)\n",
           blocks_per_cluster, blocks_per_cluster * DISKBLOCK_SIZE);
    printf("Number of raw blocks: %" PRIu32 "\n", block_count);
    printf("Data blocks on volume: %" PRIu32 "\n", data_blocks);
    printf("Number of clusters: %" PRIu32 "\n", clusters);
    printf("Size of each FAT (in blocks): %" PRIu32 "\n", blocks_per_fat);

    /* Fill in the BPB... */
    bs.boot_jmp[0] = 0xEB;              /* Short JMP */
    bs.boot_jmp[1] = 0x3C;              /* To after the BPB. */
    bs.boot_jmp[2] = 0x90;              /* NOP */
    memcpy(bs.oem_id, "KOSmkfat", 8);
    bs.bytes_per_sector[0] = 0x00;      /* 512 byte sectors. */
    bs.bytes_per_sector[1] = 0x02;
    bs.sectors_per_cluster = blocks_per_cluster;
    bs.reserved_sectors = 32;
    bs.fat_copies = 2;
    bs.unused[0] = 0;
    bs.unused[1] = 0;
    bs.unused[2] = 0;
    bs.unused[3] = 0;
    bs.media_code = 0xF8;               /* 0xF8 = Not a floppy disk */
    bs.unused2 = 0;
    bs.sectors_per_track = 1;           /* We don't care about CHS addressing */
    bs.heads = 1;                       /* Ditto. */
    bs.hidden_sectors = 0;
    bs.num_sectors = block_count;
    bs.sectors_per_fat = blocks_per_fat;
    bs.flags = 0;                       /* Mirror the FAT, as FAT16 does */
    bs.version = 0;                     /* Version 0.0 */
    bs.rootdir_cluster = 2;
    bs.fs_info = 1;
    bs.backup_boot = 6;                 /* The standard place to put it... */
    memset(bs.reserved, 0, 12);
    bs.drive_number = 0x80;             /* Fixed disk */
    bs.chkdsk_needed = 0;
    bs.ext_sig = 0x29;                  /* Magic value, required by FAT */
    bs.serial[0] = now & 0xFF;          /* Set the serial number to the time */
    bs.serial[1] = (now >> 8) & 0xFF;
    bs.serial[2] = (now >> 16) & 0xFF;
    bs.serial[3] = (now >> 24) & 0xFF;
    memcpy(bs.vol_name, "NO NAME    ", 11);
    memcpy(bs.fs_type, "FAT32   ", 8);
    bs.boot_code[0] = 0xCD;             /* INT 18h */
    bs.boot_code[1] = 0x18;
    bs.boot_code[2] = 0xEB;             /* JMP -2 (an infinite loop) */
    bs.boot_code[3] = 0xFE;
    memset(&bs.boot_code[4], 0, 416);
    bs.block_sig[0] = 0x55;             /* Magic value, required by FAT */
    bs.block_sig[1] = 0xAA;

    /* Fill in the filesystem information sector... */
    is.magic1 = 0x41615252;
    memset(is.reserved, 0, 480);
    is.magic2 = 0x61417272;
    is.free_clusters = clusters - 1;    /* Only the root directory is used. */
    is.used_cluster = 2;
    memset(is.reserved2, 0, 12);
    is.block_sig[0] = 0;
    is.block_sig[1] = 0;
    is.block_sig[2] = 0x55;
    is.block_sig[3] = 0xAA;

    /* TODO: It'd be nice to align the data area on a nice cluster boundary (or
       at least on a 4KiB boundary), but that isn't strictly necessary. */

    /* Now, all that is left is actually writing data out to the disk... */
    memcpy(block, &bs, 512);
    bd->write_blocks(bd, 0, 1, block);  /* Block 0 = BPB. */
    bd->write_blocks(bd, 6, 1, block);  /* Block 6 = Backup of BPB. */

    memcpy(block, &is, 512);
    bd->write_blocks(bd, 1, 1, block);  /* Block 1 = FS Info block */
    bd->write_blocks(bd, 7, 1, block);  /* Block 7 = (unused) FS Info backup */

    /* Now, fill in the first three entries of the FAT so we can write the two
       copies of the FAT to the disk. */
    memset(block + 12, 0, 500);
    block[0] = 0xF8;
    block[1] = 0xFF;
    block[2] = 0xFF;
    block[3] = 0x0F;
    block[4] = 0xFF;
    block[5] = 0xFF;
    block[6] = 0xFF;
    block[7] = 0x0F;
    block[8] = 0xFF;                    /* FAT[2] = Root directory */
    block[9] = 0xFF;
    block[10] = 0xFF;
    block[11] = 0x0F;
    bd->write_blocks(bd, reserved_blocks, 1, block);
    bd->write_blocks(bd, reserved_blocks + blocks_per_fat, 1, block);

    /* Clear the three entries and fill in the rest of the FATs. */
    memset(block, 0, 12);
    for(i = 1; i < blocks_per_fat; ++i) {
        bd->write_blocks(bd, reserved_blocks + i, 1, block);
        bd->write_blocks(bd, reserved_blocks + i + blocks_per_fat, 1, block);
    }

    /* Zero out the root directory's cluster. */
    reserved_blocks += 2 * blocks_per_fat;
    for(i = 0; i < blocks_per_cluster; ++i) {
        bd->write_blocks(bd, reserved_blocks + i, 1, block);
    }

    /* And, we're done. */
    return 0;
}

/******************************************************************************
                            Magic Glue Code
                        Don't touch this block.
 ******************************************************************************/
static void __attribute__((__noreturn__)) exit_with_error(const char *err) {
#ifdef _arch_dreamcast
    maple_device_t *dev;
    cont_state_t *state;

    printf("%s\n\nPress any button to exit.\n", err);

    for(;;) {
        dev = maple_enum_type(0, MAPLE_FUNC_CONTROLLER);

        if(dev) {
            state = (cont_state_t *)maple_dev_status(dev);

            if(state)   {
                if(state->buttons)
                    arch_exit();
            }
        }
    }
#else
    printf("%s\n", err);
    exit(EXIT_FAILURE);
#endif
}

/* For use outside of KOS... */
#ifndef _arch_dreamcast
static int blockdev_dummy(kos_blockdev_t *d) {
    (void)d;
    return 0;
}

static int blockdev_shutdown(kos_blockdev_t *d) {
    FILE *fp = (FILE *)d->dev_data;

    fclose(fp);
    return 0;
}

static int blockdev_read(kos_blockdev_t *d, uint32_t block, size_t count,
                         void *buf) {
    FILE *fp = (FILE *)d->dev_data;
    int fd = fileno(fp);
    ssize_t rv;

    rv = pread(fd, buf, count << d->l_block_size, block << d->l_block_size);
    return (rv > 0) ? 0 : -1;
}

static int blockdev_write(kos_blockdev_t *d, uint32_t block, size_t count,
                   const void *buf) {
    FILE *fp = (FILE *)d->dev_data;
    int fd = fileno(fp);
    ssize_t rv;

    rv = pwrite(fd, buf, count << d->l_block_size, block << d->l_block_size);
    return (rv > 0) ? 0 : -1;
}

static uint32_t blockdev_count(kos_blockdev_t *d) {
    FILE *fp = (FILE *)d->dev_data;
    off_t len;

    fseeko(fp, 0, SEEK_END);
    len = ftello(fp);
    fseeko(fp, 0, SEEK_SET);

    return (uint32_t)(len >> d->l_block_size);
}

static kos_blockdev_t the_bd = {
    NULL,
    9,

    &blockdev_dummy,
    &blockdev_shutdown,

    &blockdev_read,
    &blockdev_write,
    &blockdev_count
};

static int sd_init(void) {
    return 0;
}

static void sd_shutdown(void) {
}
#endif
/******************************************************************************
                          End Magic Glue Code
 ******************************************************************************/

int main(int argc, char *argv[]) {
    kos_blockdev_t sd_dev;
    uint8_t partition_type;
    uint32_t block_count;
    uint64_t partition_size;
    int fs_type;
    int rv;

    /* Use framebuffer debug output. */
#ifdef _arch_dreamcast
    dbgio_dev_select("fb");
#endif

    if(sd_init()) {
        exit_with_error("Could not initialize the SD card.\n"
                        "Please make sure that you have an SD card\n"
                        "adapter plugged in and an SD card inserted.");
    }

#ifdef _arch_dreamcast
    (void)argc;
    (void)argv;

    /* Grab the block device for the first partition on the SD card. Note that
       you must have the SD card formatted with an MBR partitioning scheme. */
    if(sd_blockdev_for_partition(0, &sd_dev, &partition_type)) {
        sd_shutdown();
        exit_with_error("Could not find the first partition on the SD card!\n");
    }

    /* Read the MBR so we can change the partition type if needed. */
    if(sd_read_blocks(0, 1, block)) {
        sd_shutdown();
        exit_with_error("Cannot read the MBR from the SD card!\n");
    }

    /* If it isn't already set to 0x0C (FAT32), set it to 0x0C.
       Note: We really should base this on how many clusters we'll end up
       having in the filesystem, but I'm lazy and don't feel like worrying with
       that little issue. Perhaps at a later time I will. */
    if(block[0x01BE + 4] != 0x0C) {
        printf("Partition type 0x%02x will be replaced by 0x0C\n",
               block[0x01BE + 4]);
        block[0x01BE + 4] = 0x0C;

        if(sd_write_blocks(0, 1, block)) {
            sd_shutdown();
            exit_with_error("Cannot write MBR back to the SD card!\n");
        }
    }
#else
    (void)partition_type;

    if(argc != 2)
        exit_with_error("Must supply an image filename!\n");

    /* Whee... random scopes for fun and profit! */
    {
        FILE *fp = fopen(argv[1], "r+b");
        if(!fp) {
            exit_with_error("Cannot open filesystem image file\n");
        }

        the_bd.dev_data = fp;
        sd_dev = the_bd;
    }
#endif

    /* Initialize the block device. */
    if(sd_dev.init(&sd_dev)) {
        sd_shutdown();
        exit_with_error("Could not initialize the block device!\n");
    }

    /* Figure out how large the partition is, so we know how big to make our
       filesystem blocks. */
    block_count = sd_dev.count_blocks(&sd_dev);
    partition_size = (uint64_t)(1 << sd_dev.l_block_size) * block_count;

    printf("%" PRIu32 " raw blocks in partition (%d bytes each)\n", block_count,
           1 << sd_dev.l_block_size);
    printf("Detected partition size of %" PRIu64 " bytes\n", partition_size);

    /* Figure out what filesystem we'll try to create... Note that this code
       doesn't actually support FAT12 at the moment, so there is a minimum
       filesystem size that wouldn't be here if we did... */
    if(partition_size < 5 * MiB) {
        sd_shutdown();
        exit_with_error("Cowardly refusing to create filesystem of less than "
                        "5MiB.\n");
    }
    /* Arbitrarily say that FAT16 volumes are between 5MiB and 512MiB. */
    else if(partition_size < 512 * MiB) {
        fs_type = FAT16;
    }
    /* Everything at least 512MiB in length is FAT32 for the purposes of this
       tool. */
    else {
        fs_type = FAT32;
    }

    printf("Going to create a %s volume.\n", fs_names[fs_type - 1]);

    /* Now that we know what type of FS to create, the rest is pretty much FS
       dependent... */
    switch(fs_type) {
        case FAT16:
            rv = format_fat16(&sd_dev, block_count, partition_size);
            break;
        case FAT32:
            rv = format_fat32(&sd_dev, block_count, partition_size);
            break;
    }

    if(rv)
        printf("Format failed...\n");
    else
        printf("Format complete!\n");

    sd_dev.shutdown(&sd_dev);
    sd_shutdown();

    return 0;
}
