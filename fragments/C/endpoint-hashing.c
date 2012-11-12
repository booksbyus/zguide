uint16_t hash = 0;
while (*endpoint)
    hash = 33 * hash ^ *endpoint++;
