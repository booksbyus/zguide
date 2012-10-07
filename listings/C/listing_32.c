client_execute (client_t *self, int event)
{
    self->next_event = event;
    while (self->next_event) {
        self->event = self->next_event;
        self->next_event = 0;
        switch (self->state) {
.for class.state
            case $(name:c)_state:
.   for event
.       if index () > 1
                else
.       endif
                if (self->event == $(name:c)_event) {
.       for action
.           if name = "send"
                    zmsg_addstr (self->reply, "$(message:)");
.           else
                $(name:c)_action (self);
.           endif
.       endfor
.       if defined (event.next)
                    self->state = $(next:c)_state;
.       endif
                }
.   endfor
                break;
.endfor
        }
        if (zmsg_size (self->reply) > 1) {
            zmsg_send (&self->reply, self->router);
            self->reply = zmsg_new ();
            zmsg_add (self->reply, zframe_dup (self->address));
        }
    }
}
