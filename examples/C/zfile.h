/*  =========================================================================
    zfile.h - helper functions for working with files.

    -------------------------------------------------------------------------
    Copyright (c) 1991-2011 iMatix Corporation <www.imatix.com>
    Copyright other contributors as noted in the AUTHORS file.

    This file is part of the ZeroMQ Guide: http://zguide.zeromq.org

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

#ifndef __ZFILE_H_INCLUDED__
#define __ZFILE_H_INCLUDED__


#if (!defined (__WINDOWS__))
#   include <sys/stat.h>
#   include <sys/types.h>
#endif

//  Delete file, return 0 if OK, -1 if not possible. Does not require that
//  file is present.

int
file_delete (char *filename)
{
    assert (filename);
#if (defined (__WINDOWS__))
    return !DeleteFile (filename);
#else
    return unlink (filename);
#endif
}


//  Make directory (maximum one level depending on OS)

int
file_mkdir (char *dirname)
{
#if (defined (__WINDOWS__))
    return !CreateDirectory (dirname, NULL));
#else
    return mkdir (dirname, 0755);    //  User RWE Group RE World RE
#endif
}


//  Returns the file mode for the specified file or directory name;
//  returns 0 if the specified file does not exist.

static int
file_mode (char *filename)
{
    assert (filename);
    
#if (defined (__WINDOWS__))
    DWORD dwfa = GetFileAttributes (filename);
    if (dwfa == 0xffffffff)
        return 0;
    
    int mode = 0;
    if (dwfa & FILE_ATTRIBUTE_DIRECTORY)
        mode |= S_IFDIR;
    else
        mode |= S_IFREG;
    
    if (!(dwfa & FILE_ATTRIBUTE_HIDDEN))
        mode |= S_IREAD;
    
    if (!(dwfa & FILE_ATTRIBUTE_READONLY))
        mode |= S_IWRITE;

    return mode;
#else
    struct stat stat_buf;
    if (stat ((char *) filename, &stat_buf) == 0)
        return stat_buf.st_mode;
    else
        return 0;
#endif
}

//  Return 1 if file exists, else zero
int
file_exists (char *filename)
{
    assert (filename);
    return (file_mode (filename) > 0);
}


#endif  //  __ZFILE_H_INCLUDED__
