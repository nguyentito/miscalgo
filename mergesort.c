#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <pthread.h>

#include "stopwatch.h"

#define ARRAY_SIZE (2013*2013*4)

typedef struct list list;
struct list {
    int x;
    list *next;
};

void list_free(list *l) {
    list *q, *p = l;
    while (p != NULL) {
        q = p->next;
        free(p);
        p = q;
    }
}

list* list_cons(int x, list *t) {
    list *l;
    l = malloc(sizeof(list));
    l->x = x;
    l->next = t;
    return l;
}

/* Note : mergesort is destructive (in-place), and returns the pointer
          to the new first element
          O(log n) space, O(n log n) time */

list* merge(list *a, list *b) {
    list *head, *p, **q;

    if (a == NULL) return b;
    if (b == NULL) return a;
    if (a->x <= b->x) {
        head = a;
        a = a->next;
    } else {
        head = b;
        b = b->next;
    }

    /* looks like a normal list traversal, but actually,
       the list is gradually built one step ahead */
    for (p = head; a != NULL && b != NULL; p = p->next) {
        if (a->x <= b->x) {
            p->next = a;
            a = a->next;
        } else {
            p->next = b;
            b = b->next;
        }
    }
    
    p->next = (a == NULL ? b : a);
    return head;
}

list* merge_sort(list *l) {
    list *even, *odd, *p, *q;

    if (l == NULL || l->next == NULL) return l;

    even = l; odd = l->next;
    p = even; q = odd;
    for (;;) {
        p = (p->next = q->next);
        if (p == NULL) break;
        q = (q->next = p->next);
        if (q == NULL) break;
    }
    if (p != NULL)
        p->next = NULL;
    else
        q->next = NULL;
    
    return merge(merge_sort(even), merge_sort(odd));
}

list* array_to_list(int n, int *t) {
    int i;
    list *l = NULL;
    for (i = n-1; i >= 0; i--) {
        l = list_cons(t[i], l);
    }
    return l;
}

int* random_array(int n) {
    int i;
    int *t;
    t = malloc(n*sizeof(int));
    for (i = 0; i < n; i++) {
        t[i] = rand() % 65536;
    }
    return t;
}

/* function pointer to provide to qsort,
   hence the const void* types */
int compare_int(const void * a, const void * b) {
    return ( *(int*)a - *(int*)b );
}

/* assumes the list and the array have the same length
   if len(l) > len(t), it may segfault ! */
int list_array_equal(list *l, int *t) {
    int i;
    list *p;
    for (i = 0, p = l; p != NULL; i++, p = p->next) {
        if (t[i] != p->x) return 0;
    }
    return 1;
}

void print_list(list *l) {
    list *p;
    for (p = l; p != NULL; p = p->next) {
        printf("%d\n", p->x);
    }
}


/************** Here comes the interesting part *************/

struct mspa_args {
    list *l;
    int depth;
};

list* merge_sort_parallel_aux(struct mspa_args *args) {
    list *l, *even, *odd, *p, *q;
    l = args->l;

    if (l == NULL || l->next == NULL) return l;

    /* copied straight from the sequential version */
    even = l; odd = l->next;
    p = even; q = odd;
    for (;;) {
        p = (p->next = q->next);
        if (p == NULL) break;
        q = (q->next = p->next);
        if (q == NULL) break;
    }
    if (p != NULL)
        p->next = NULL;
    else
        q->next = NULL;
    
    /* decide to execute sequentially or create a thread
       depending on the current depth of recursion
       max depth 3 = 8 threads
    */
    if (args->depth >= 3) {
        return merge(merge_sort(even), merge_sort(odd));
    } else {
        list *a, *b;
        pthread_t thread;
        struct mspa_args thread_args;

        /* TODO : check return values for errors
           for pthread_create and pthread_join
         */

        thread_args.l = odd;
        thread_args.depth = args->depth + 1;
        pthread_create(&thread, NULL,
                       (void * (*)(void *))merge_sort_parallel_aux,
                       &thread_args);

        args->depth++;
        args->l = even;
        a = merge_sort_parallel_aux(args);

        pthread_join(thread, (void*)&b);
        return merge(a,b);
    }
}

list* merge_sort_parallel(list *l) {
    struct mspa_args args;
    args.l = l;
    args.depth = 0;
    return merge_sort_parallel_aux(&args);
}


/*** Displaying and benchmarking ***/

int main(int argc, char *argv[]) {
    struct timespec before, after;
    int *t;
    list *l1, *l2;
    
    srand(time(NULL));

    t = random_array(ARRAY_SIZE);
    l1 = array_to_list(ARRAY_SIZE, t);
    l2 = array_to_list(ARRAY_SIZE, t);

    printf("Starting\n");
    
    sw_reset();
    qsort(t, ARRAY_SIZE, sizeof(int), compare_int);
    printf("Stdlib quicksort finished in %ld milliseconds\n",
           sw_get_time());

    sw_reset();
    l1 = merge_sort(l1);
    printf("Sequential mergesort finished in %ld milliseconds\n",
           sw_get_time());

    sw_reset();
    l2 = merge_sort_parallel(l2);
    printf("Parallel mergesort finished in %ld milliseconds\n",
           sw_get_time());

    if (list_array_equal(l2,t)) {
        printf("Success!\n");
    } else {
        printf("Failure :-(\n");
    }
    
    free(t);
    free(l1);
    free(l2);
    return 0;
}
