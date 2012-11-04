## The MDP/Client Protocol

A REQUEST command consists of a multi-part message of 4
frames:

* Frame 1: "" (0 bytes, Empty frame)
* Frame 2: "MDPC01" (6 bytes, Protocol identifier)
* Frame 3: Service name (printable string)
* Frame 4: Request body (opaque binary)

A REPLY command consists of a multi-part message of 4
frames:

* Frame 1: "" (0 bytes, Empty frame)
* Frame 2: "MDPC01" (6 bytes, Protocol identifier)
* Frame 3: Service name (printable string)
* Frame 4: Response body (opaque binary)
