//
//  Device template
//  http://www.jsonlint.com/
//
#include <zmq.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <assert.h>
#include "cJSON.h"

int main (int argc, char *argv [])
{
    //  Read configuration from file
    char *config_name = "device.json";
    FILE *config_file;
    char *config_data;
    long  config_size;

    config_file = fopen (config_name, "rb");
    fseek (config_file, 0, SEEK_END);
    config_size = ftell (config_file);
    config_data = malloc (config_size + 1);
    fseek (config_file, 0, SEEK_SET);
    assert (fread (config_data, 1, config_size, config_file) == config_size);
    fclose (config_file);

    //  Parse JSON data
    cJSON *json;

    json = cJSON_Parse (config_data);
    if (json == NULL) {
        printf ("E: '%s' is not valid JSON\n", config_name);
        exit (1);
    }
    cJSON_Delete (json);

    return 0;
}
