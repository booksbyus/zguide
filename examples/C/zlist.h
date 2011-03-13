/*  =========================================================================
    zlist.class - import of ZFL singly-linked list class

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

#ifndef __ZLIST_INCLUDED__
#define __ZLIST_INCLUDED__

#include "zhelpers.h"

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _zlist zlist_t;

zlist_t *
    zlist_new (void);
void
    zlist_destroy (zlist_t **self_p);
void *
    zlist_first (zlist_t *self);
void *
    zlist_next (zlist_t *self);
void
    zlist_append (zlist_t *self, void *value);
void
    zlist_push (zlist_t *self, void *value);
void *
    zlist_pop (zlist_t *self);
void
    zlist_remove (zlist_t *self, void *value);
zlist_t *
    zlist_copy (zlist_t *self);
size_t
    zlist_size (zlist_t *self);
void
    zlist_test (int verbose);

#ifdef __cplusplus
}
#endif

//  Macros and typedefs defined by the ZFL header
#define zmalloc(size) calloc(1,(size))


/*  =========================================================================
    zlist.c - singly-linked list container

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

//  List node, used internally only

struct node_t {
    struct node_t
        *next;
    void
        *value;
};

//  Actual list object

struct _zlist {
    struct node_t
        *head, *tail;
    struct node_t
        *cursor;
    size_t
        size;
};


//  --------------------------------------------------------------------------
//  List constructor

zlist_t *
zlist_new (void)
{
    zlist_t *self = (zlist_t *) zmalloc (sizeof (zlist_t));
    return self;
}


//  --------------------------------------------------------------------------
//  List destructor

void
zlist_destroy (zlist_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        zlist_t *self = *self_p;
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
//  Leaves cursor pointing at the head item, or NULL if the list is empty.

void *
zlist_first (zlist_t *self)
{
    assert (self);
    self->cursor = self->head;
    if (self->head)
        return self->head->value;
    else
        return NULL;
}


//  --------------------------------------------------------------------------
//  Return the next value. If the list is empty, returns NULL. To move to
//  the start of the list call zlist_first(). Advances the cursor.

void *
zlist_next (zlist_t *self)
{
    assert (self);
    if (self->cursor)
        self->cursor = self->cursor->next;
    if (self->cursor)
        return self->cursor->value;
    else
        return NULL;
}


//  --------------------------------------------------------------------------
//  Add value to the end of the list

void
zlist_append (zlist_t *self, void *value)
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
    self->cursor = NULL;
}


//  --------------------------------------------------------------------------
//  Insert value at the beginning of the list

void
zlist_push (zlist_t *self, void *value)
{
    struct node_t *node;
    node = (struct node_t *) zmalloc (sizeof (struct node_t));
    node->value = value;
    node->next = self->head;
    self->head = node;
    if (self->tail == NULL)
        self->tail = node;
    self->size++;
    self->cursor = NULL;
}


//  --------------------------------------------------------------------------
//  Remove value from the beginning of the list, returns NULL if none

void *
zlist_pop (zlist_t *self)
{
    struct node_t *node = self->head;
    void *value = NULL;
    if (node) {
        value = node->value;
        self->head = node->next;
        if (self->tail == node)
            self->tail = NULL;
        free (node);
    }
    self->size--;
    self->cursor = NULL;
    return value;
}


//  --------------------------------------------------------------------------
//  Remove the value from the list, if present. Safe to call on values that
//  are not in the list.

void
zlist_remove (zlist_t *self, void *value)
{
    struct node_t *node, *prev = NULL;

    //  First off, we need to find the list node.
    for (node = self->head; node != NULL; node = node->next) {
        if (node->value == value)
            break;
        prev = node;
    }
    if (node) {
        if (prev)
            prev->next = node->next;
        else
            self->head = node->next;

        if (node->next == NULL)
            self->tail = prev;

        free (node);
        self->size--;
        self->cursor = NULL;
    }
}


//  --------------------------------------------------------------------------
//  Make copy of itself

zlist_t *
zlist_copy (zlist_t *self)
{
    if (!self)
        return NULL;

    zlist_t *copy = zlist_new ();
    assert (copy);

    struct node_t *node;
    for (node = self->head; node; node = node->next)
        zlist_append (copy, node->value);
    return copy;
}


//  --------------------------------------------------------------------------
//  Return the number of items in the list

size_t
zlist_size (zlist_t *self)
{
    return self->size;
}


//  --------------------------------------------------------------------------
//  Runs selftest of class

void
zlist_test (int verbose)
{
    printf (" * zlist: ");

    zlist_t *list = zlist_new ();
    assert (list);
    assert (zlist_size (list) == 0);

    //  Three values we'll use as test data
    //  List values are void *, not particularly strings
    char *cheese = "boursin";
    char *bread = "baguette";
    char *wine = "bordeaux";

    zlist_append (list, cheese);
    assert (zlist_size (list) == 1);
    zlist_append (list, bread);
    assert (zlist_size (list) == 2);
    zlist_append (list, wine);
    assert (zlist_size (list) == 3);

    assert (zlist_first (list) == cheese);
    assert (zlist_next (list) == bread);
    assert (zlist_next (list) == wine);
    assert (zlist_size (list) == 3);

    zlist_remove (list, wine);
    assert (zlist_size (list) == 2);

    assert (zlist_first (list) == cheese);
    zlist_remove (list, cheese);
    assert (zlist_size (list) == 1);
    assert (zlist_first (list) == bread);

    zlist_remove (list, bread);
    assert (zlist_size (list) == 0);

    zlist_push (list, cheese);
    assert (zlist_size (list) == 1);
    assert (zlist_first (list) == cheese);

    zlist_push (list, bread);
    assert (zlist_size (list) == 2);
    assert (zlist_first (list) == bread);

    zlist_append (list, wine);
    assert (zlist_size (list) == 3);
    assert (zlist_first (list) == bread);

    zlist_remove (list, bread);
    assert (zlist_first (list) == cheese);

    zlist_remove (list, cheese);
    assert (zlist_first (list) == wine);

    zlist_remove (list, wine);
    assert (zlist_size (list) == 0);

    //  Destructor should be safe to call twice
    zlist_destroy (&list);
    zlist_destroy (&list);
    assert (list == NULL);

    printf ("OK\n");
}

#endif
