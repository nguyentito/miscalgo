#include "stopwatch.h"

#include <time.h>
#include <sys/time.h> /* POSIX only */

/* Dazzling the stage ! With static global vars */
static struct timespec zero_time;

long time_diff_ms(struct timespec *a, struct timespec *b) {
    return (a->tv_sec - b->tv_sec)*1000 + (a->tv_nsec - b->tv_nsec)/1000000;
}

void sw_reset() {
    clock_gettime(CLOCK_MONOTONIC, &zero_time);
}

long sw_get_time() {
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    return time_diff_ms(&now, &zero_time);
}
