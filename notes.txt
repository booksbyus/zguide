Chapter 5:

    -> how to make async APIs
    -> adapting Clone for Zyre
    -> state in HELLO
    -> update messages

- hacks
    - mass opening

+++ Security

+++ Connecting over Internet

* public IP addresses and testing reachability
* security
* bridging







==1742== Syscall param write(buf) points to uninitialised byte(s)
==1742==    at 0x5372100: __write_nocancel (syscall-template.S:82)
==1742==    by 0x5306312: _IO_file_write@@GLIBC_2.2.5 (fileops.c:1289)
==1742==    by 0x53061D9: new_do_write (fileops.c:543)
==1742==    by 0x5307944: _IO_do_write@@GLIBC_2.2.5 (fileops.c:516)
==1742==    by 0x5306E6F: _IO_file_close_it@@GLIBC_2.2.5 (fileops.c:170)
==1742==    by 0x52FB2CF: fclose@@GLIBC_2.2.5 (iofclose.c:62)
==1742==    by 0x4060FF: fmq_file_test (fmq_file.c:342)
==1742==    by 0x4021E6: main (fmq_selftest.c:38)
==1742==  Address 0x4027000 is not stack'd, malloc'd or (recently) free'd

    FILE *handle = fopen ("testdata", "w");
    zmq_msg_t msg;
    zmq_msg_init_size (&msg, 100);
    size_t items = fwrite (zmq_msg_data (&msg), 1, 100, handle);
    fclose (handle);
    zmq_msg_close (&msg);

+++ Bridging

- why and how
- semantic compatibility
- example 0MQ to HTTP
- using zxxx library?

+++ Disconnected Security
    
Chapter 9 topics

- the wire level protocol
- a minimal TCP stack
- internals of 0MQ
- generate bindings in
    Python
    C#
    Java
    Ruby



++++ 

0MQ's pipeline pattern (using PUSH and PULL sockets) is reliable to the extent that:

* Workers and collectors don't crash;
* Workers and collectors read their data fast enough to avoid queue overflows.

As with all our reliability patterns, we'll ignore what happens if an upstream node (the ventilator for a pipeline pattern) dies. In practice a ventilator will be the client of another reliability pattern, e.g. Clone.

The Xyz pattern takes pipeline and makes it robust against the only failure we can reasonably handle, namely workers and (less commonly) collectors that crash and lose messages or work.

- assume workers are idempotent
- assume batch size is known in advance (because...)
- assume memory enough to hold full batch
- batch: start (address of collector), tasks, end
- messages numbered 0 upwards inside batch
- assume multiple ventilators for same cluster
- assume collector talks to ventilator, (not same to allow walk-up-and use by ventilators)
- call ventilator the 'client'
- if task missing, resend
- if end of batch missing, resend from last response


/*
 * Threadsafe basename implementation
 *
 * Returns the path component following the final slash ('/'), excluding
 * any trailing slashes, which are dropped. If the pathname does not
 * contain a slash, returns NULL (unlike POSIX basename which returns the
 * whole path). And which is not re-entrant. If successful, allocates a
 * new string that the caller must free when done with.
 */

char *safe_basename (char *path)
{
    if (!path)
        return NULL;

    //  Strip off any trailing slashes
    char *path_copy = strdup (path);
    while (path_copy [strlen (path_copy) - 1] == '/')
        path_copy [strlen (path_copy) - 1] = '\0';

    //  Find last slash and take filename after that
    char *last_slash = strrchr (path_copy, '/');
    char *filename = NULL;
    if (last_slash && last_slash > path_copy)
        filename = strdup (last_slash + 1);
    else
    //  Or, take the whole filename unless it's dotted
    if (*path_copy && *path_copy != '.')
        filename = strdup (path_copy);

    free (path_copy);
    return filename;
}

