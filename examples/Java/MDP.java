/**
 * (c) 2011 Arkadiusz Orzechowski
 *
 * This file is part of ZGuide
 *
 * ZGuide is free software; you can redistribute it and/or modify it under
 * the terms of the Lesser GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * ZGuide is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * Lesser GNU General Public License for more details.
 *
 * You should have received a copy of the Lesser GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
import java.util.Arrays;

import org.zeromq.ZFrame;

/**
 * Majordomo Protocol definitions, Java version
 * 
 * @author Arkadiusz Orzechowski <aorzecho@gmail.com>
 */
public enum MDP {

    /**
     * This is the version of MDP/Client we implement
     */
    C_CLIENT("MDPC01"),

    /**
     * This is the version of MDP/Worker we implement
     */
    W_WORKER("MDPW01"),

    // MDP/Server commands, as byte values
    W_READY(1), 
    W_REQUEST(2), 
    W_REPLY(3), 
    W_HEARTBEAT(4), 
    W_DISCONNECT(5);

    private final byte[] data;

    MDP(String value) {
        this.data = value.getBytes();
    }
    MDP(int value) { //watch for ints>255, will be truncated
        byte b = (byte) (value & 0xFF);
        this.data = new byte[] { b };
    }

    public ZFrame newFrame () {
        return new ZFrame(data);
    }

    public boolean frameEquals (ZFrame frame) {
        return Arrays.equals(data, frame.getData());
    }
}
