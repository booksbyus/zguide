//  Publish file into virtual space
void
zre_interface_publish (zre_interface_t *self,
                       char *filename, char *external)
{
    zstr_sendm (self->pipe, "PUBLISH");
    zstr_sendm (self->pipe, filename);  //  Real file name
    zstr_send  (self->pipe, external);  //  Location in virtual space
}
