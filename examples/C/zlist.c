/*  =========================================================================
    zfl_list.h - ZFL singly-linked list class

    -------------------------------------------------------------------------
    Copyright (c) 1991-2011 iMatix Corporation <www.imatix.com>
    Copyright other contributors as noted in the AUTHORS file.

    This file is part of the ZeroMQ Function Library: http://zfl.zeromq.org

    This is free software; you can redistribute it and/or modify it under the
    terms of the GNU Lesser General Public License as published by the Free
    Software Foundation; either version 3 of the License, or (at your option)
    any later version.

    This software is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABIL-
    ITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General
    Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
    =========================================================================
*/

#ifndef __ZFL_LIST_H_INCLUDED__
#define __ZFL_LIST_H_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _zfl_list zfl_list_t;

zfl_list_t *
    zfl_list_new (void);
void
    zfl_list_destroy (zfl_list_t **self_p);
void *
    zfl_list_first (zfl_list_t *self);
void
    zfl_list_append (zfl_list_t *self, void *value);
void
    zfl_list_push (zfl_list_t *self, void *value);
void
    zfl_list_remove (zfl_list_t *self, void *value);
zfl_list_t *
    zfl_list_copy (zfl_list_t *self);
size_t
    zfl_list_size (zfl_list_t *self);
void
    zfl_list_test (int verbose);

#ifdef __cplusplus
}
#endif

#endif
/*  =========================================================================
    zfl_list.c - singly-linked list container

    Provides a generic container implementing a fast singly-linked list. You
    can use this to construct multi-dimensional lists, and other structures
    together with other generic containers like zfl_hash.

    -------------------------------------------------------------------------
    Copyright (c) 1991-2011 iMatix Corporation <www.imatix.com>
    Copyright other contributors as noted in the AUTHORS file.

    This file is part of the ZeroMQ Function Library: http://zfl.zeromq.org

    This is free software; you can redistribute it and/or modify it under the
    terms of the GNU Lesser General Public License as published by the Free
    Software Foundation; either version 3 of the License, or (at your option)
    any later version.

    This software is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABIL-
    ITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General
    Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
    =========================================================================
*/

#include "../include/zfl_prelude.h"
#include "../include/zfl_list.h"

//  List node, used internally only

struct node_t {
    struct node_t
        *next;
    void
        *value;
};

//  Actual list object

struct _zfl_list {
    struct node_t
        *head, *tail;
    size_t
        size;
};


//  --------------------------------------------------------------------------
//  List constructor

zfl_list_t *
zfl_list_new (void)
{
    zfl_list_t *self = (zfl_list_t *) zmalloc (sizeof (zfl_list_t));
    return self;
}


//  --------------------------------------------------------------------------
//  List destructor

void
zfl_list_destroy (zfl_list_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        zfl_list_t *self = *self_p;
        struct node_t *node, *next;
        for (node = (*self_p)->head; node != NULL; node = next) {
            next = node->next;
            free (node);
        }
        free (self);
        *self_p = NULL;
    }
}


//  --------------------------------------------------------------------------
//  Return the value at the head of list. If the list is empty, returns NULL.
//  Note that this function does not remove the value from the list.

void *
zfl_list_first (zfl_list_t *self)
{
    assert (self);
    if (self->head)
        return self->head->value;
    else
        return NULL;
}


//  --------------------------------------------------------------------------
//  Add value to the end of the list

void
zfl_list_append (zfl_list_t *self, void *value)
{
    struct node_t *node;
    node = (struct node_t *) zmalloc (sizeof (struct node_t));
    node->value = value;
    if (self->tail)
        self->tail->next = node;
    else
        self->head = node;
    self->tail = node;
    node->next = NULL;
    self->size++;
}


//  --------------------------------------------------------------------------
//  Insert value at the beginning of the list

void
zfl_list_push (zfl_list_t *self, void *value)
{
    struct node_t *node;
    node = (struct node_t *) zmalloc (sizeof (struct node_t));
    node->value = value;
    node->next = self->head;
    self->head = node;
    if (self->tail == NULL)
        self->tail = node;
    self->size++;
}


//  --------------------------------------------------------------------------
//  Remove the value value from the list. The value must be stored in the list.
//  The function does not deallocate the memory pointed to by the removed value.

void
zfl_list_remove (zfl_list_t *self, void *value)
{
    struct node_t *node, *prev = NULL;

    //  First off, we need to find the list node.
    for (node = self->head; node != NULL; node = node->next) {
        if (node->value == value)
            break;
        prev = node;
    }
    assert (node);

    if (prev)
        prev->next = node->next;
    else
        self->head = node->next;

    if (node->next == NULL)
        self->tail = prev;

    free (node);
    self->size--;
}


//  --------------------------------------------------------------------------
//  Make copy of itself

zfl_list_t *
zfl_list_copy (zfl_list_t *self)
{
    if (!self)
        return NULL;

    zfl_list_t *copy = zfl_list_new ();
    assert (copy);

    struct node_t *node;
    for (node = self->head; node; node = node->next)
        zfl_list_append (copy, node->value);
    return copy;
}


//  --------------------------------------------------------------------------
//  Return the number of items in the list

size_t
zfl_list_size (zfl_list_t *self)
{
    return self->size;
}


//  --------------------------------------------------------------------------
//  Runs selftest of class

void
zfl_list_test (int verbose)
{
    printf (" * zfl_list: ");

    zfl_list_t *list = zfl_list_new ();
    assert (list);
    assert (zfl_list_size (list) == 0);

    //  Three values we'll use as test data
    //  List values are void *, not particularly strings
    char *cheese = "boursin";
    char *bread = "baguette";
    char *wine = "bordeaux";

    zfl_list_append (list, cheese);
    assert (zfl_list_size (list) == 1);
    zfl_list_append (list, bread);
    assert (zfl_list_size (list) == 2);
    zfl_list_append (list, wine);
    assert (zfl_list_size (list) == 3);

    assert (zfl_list_first (list) == cheese);
    assert (zfl_list_size (list) == 3);
    zfl_list_remove (list, wine);
    assert (zfl_list_size (list) == 2);

    assert (zfl_list_first (list) == cheese);
    zfl_list_remove (list, cheese);
    assert (zfl_list_size (list) == 1);
    assert (zfl_list_first (list) == bread);

    zfl_list_remove (list, bread);
    assert (zfl_list_size (list) == 0);

    zfl_list_push (list, cheese);
    assert (zfl_list_size (list) == 1);
    assert (zfl_list_first (list) == cheese);

    zfl_list_push (list, bread);
    assert (zfl_list_size (list) == 2);
    assert (zfl_list_first (list) == bread);

    zfl_list_append (list, wine);
    assert (zfl_list_size (list) == 3);
    assert (zfl_list_first (list) == bread);

    zfl_list_remove (list, bread);
    assert (zfl_list_first (list) == cheese);

    zfl_list_remove (list, cheese);
    assert (zfl_list_first (list) == wine);

    zfl_list_remove (list, wine);
    assert (zfl_list_size (list) == 0);

    //  Destructor should be safe to call twice
    zfl_list_destroy (&list);
    zfl_list_destroy (&list);
    assert (list == NULL);

    printf ("OK\n");
}
