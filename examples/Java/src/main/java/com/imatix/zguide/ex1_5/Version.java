package com.imatix.zguide.ex1_5;

import static java.lang.String.format;
import static org.zeromq.ZMQ.getFullVersion;
import static org.zeromq.ZMQ.getVersionString;

/**
 * Report 0MQ version
 *
 * @since 1.0
 */

public class Version {

    public static void main(String[] args) {
        System.out.println(format("Version String: %s & Version int: %d",
                getVersionString(),
                getFullVersion()));
    }
}
