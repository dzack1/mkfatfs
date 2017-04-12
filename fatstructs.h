/* KallistiOS ##version##

   fatstructs.h
   Copyright (C) 2017 Lawrence Sebald
*/

/* This file contains various on-disk structures used in the variations of FAT.
   Generally, there's no reason to use these outside of the library, but it
   still made sense to me to split it out from fatinternal.h, so I did. */

#ifndef __FAT_FATSTRUCTS_H
#define __FAT_FATSTRUCTS_H

#include <stdint.h>
#include <stddef.h>

typedef struct fat16_boot {
    uint8_t boot_jmp[3];
    uint8_t oem_id[8];
    uint8_t bytes_per_sector[2]; /* Would be uint16_t, but is unaligned. */
    uint8_t sectors_per_cluster;
    uint16_t reserved_sectors;
    uint8_t fat_copies;
    uint8_t rootdir_entries[2]; /* Unaligned uint16_t */
    uint8_t num_sectors_sm[2]; /* Unaligned uint16_t */
    uint8_t media_code;
    uint16_t sectors_per_fat;
    uint16_t sectors_per_track;
    uint16_t heads;
    uint32_t hidden_sectors;
    uint32_t num_sectors_lg;
    uint8_t drive_number;
    uint8_t chkdsk_needed;
    uint8_t ext_sig;
    uint8_t serial[4];
    uint8_t vol_name[11];
    uint8_t fs_type[8];
    uint8_t boot_code[448];
    uint8_t block_sig[2];
} fat16_boot_t;

typedef struct fat32_boot {
    uint8_t boot_jmp[3];
    uint8_t oem_id[8];
    uint8_t bytes_per_sector[2]; /* Would be uint16_t, but is unaligned. */
    uint8_t sectors_per_cluster;
    uint16_t reserved_sectors;
    uint8_t fat_copies;
    uint8_t unused[4];
    uint8_t media_code;
    uint16_t unused2;
    uint16_t sectors_per_track;
    uint16_t heads;
    uint32_t hidden_sectors;
    uint32_t num_sectors;
    uint32_t sectors_per_fat;
    uint16_t flags;
    uint16_t version;
    uint32_t rootdir_cluster;
    uint16_t fs_info;
    uint16_t backup_boot;
    uint8_t reserved[12];
    uint8_t drive_number;
    uint8_t chkdsk_needed;
    uint8_t ext_sig;
    uint8_t serial[4];
    uint8_t vol_name[11];
    uint8_t fs_type[8];
    uint8_t boot_code[420];
    uint8_t block_sig[2];
} fat32_boot_t;

typedef struct fat32_info {
    uint32_t magic1;
    uint8_t reserved[480];
    uint32_t magic2;
    uint32_t free_clusters;
    uint32_t used_cluster;
    uint8_t reserved2[12];
    uint8_t block_sig[4];
} fat32_info_t;

#endif /* !__FAT_FATSTRUCTS_H */
