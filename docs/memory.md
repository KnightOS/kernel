# KnightOS Memory Layout

Memory in KnightOS is seperated into four sections - kernel code, Flash paging, kernel memory, and userspace memory.

It is laid out as follows:

<table>
    <th>Address</th><th>Length</th><th>Description</th>
    <tr><td>0x0000</td><td>0x4000</td><td>Kernel</td></tr>
    <tr><td>0x4000</td><td>0x4000</td><td>Flash paging</td></tr>
    <tr><td>0x8000</td><td>0x50</td><td>Thread table**</td></tr>
    <tr><td>0x8050</td><td>0x28</td><td>Library table**</td></tr>
    <tr><td>0x8078</td><td>0x14</td><td>Signal table**</td></tr>
    <tr><td>0x808C</td><td>0x28</td><td>File stream table**</td></tr>
    <tr><td>...</td><td>...</td><td>Various kernel variables*</td></tr>
    <tr><td>0x8100</td><td>0x100</td><td>Kernel garbage</td></tr>
    <tr><td>0x8200</td><td>0x7E00</td><td>Userspace memory</td></tr>
</table>

_\* See [defines.inc](https://github.com/KnightSoft/KnightOS/blob/master/inc/defines.inc#L66) for details_

_\*\* The size of this section could change if the maximum value is changed in [defines.inc](https://github.com/KnightSoft/KnightOS/blob/master/inc/defines.inc#L66")_

Kernel garbage is throwaway memory that the kernel uses for specific purposes for short periods of time. For example, it is used
for garbage collection, and for writing to Flash, and as temporary storage during file lookups.

Userspace memory is where all memory allocated with `malloc` is allocated to. This is where all userspace programs run.

## Data Structures

Information about kernel memory tables follows.

### Thread Table

The thread table contains state information about all currently executing threads. Each entry is 8 bytes long.

<table>
    <th>Offset</th><th>Length</th><th>Description</th>
    <tr><td>0000</td><td>1</td><td>Thread ID</td></tr>
    <tr><td>0001</td><td>2</td><td>Executable address</td></tr>
    <tr><td>0003</td><td>2</td><td>Stack pointer</td></tr>
    <tr><td>0005</td><td>1</td><td>Flags</td></tr>
    <tr><td>0006</td><td>2</td><td>Reserved for future use</td></tr>
</table>

Flags is a bitfield:

<table>
    <th>Bit</th><th>Description</th>
    <tr><td>0</td><td>May be suspended</td></tr>
    <tr><td>1</td><td>Is suspended</td></tr>
</table>

### Library Table

The library table stores information about all libraries currently loaded in the system. Each entry is 4 bytes long.

**NOTE**: This table will likely be revised to track which threads are using which libraries, and for 16 bit library IDs.

<table>
    <th>Offset</th><th>Length</th><th>Description</th>
    <tr><td>0000</td><td>1</td><td>Library ID</td></tr>
    <tr><td>0001</td><td>2</td><td>Library address</td></tr>
    <tr><td>0003</td><td>1</td><td>Number of dependent threads</td></tr>
</table>

### Signal Table

All pending signals are stored in the signal table. Each entry is 4 bytes long.

<table>
    <th>Offset</th><th>Length</th><th>Description</th>
    <tr><td>0000</td><td>1</td><td>Target thread</td></tr>
    <tr><td>0001</td><td>1</td><td>Message type</td></tr>
    <tr><td>0002</td><td>2</td><td>Payload</td></tr>
</table>

### File Stream Table

All active file streams are stored in this table.

<table>
    <th>Offset</th><th>Length</th><th>Description</th>
    <tr><td>0000</td><td>1</td><td>Flags/Owner</td></tr>
    <tr><td>0001</td><td>2</td><td>Buffer address</td></tr>
    <tr><td>0003</td><td>1</td><td>Stream pointer</td></tr>
    <tr><td>0004</td><td>2</td><td>Section identifier</td></tr>
    <tr><td>0006</td><td>1</td><td>Length of final block</td></tr>
    <tr><td>0007</td><td>1</td><td>Length of current block</td></tr>
</table>

Flags/owner is the following 8 bit format: FTExxxxx, where xxxxx is the thread ID of the owner. F is set if the stream is currently on the
final block of the file. T is set if thread is writable. E is set if the stream pointer is past the end of the file.

The buffer address is the location of the buffer in memory (the first byte). This 256-byte buffer contains the contents of the current DAT
block. The stream pointer is the offset within this buffer that the stream is currently poitned to. When this offset overflows or underflows,
the system copies the next or prior block (respectively) into the buffer. For writable streams, the stream is first flushed.

The section identifer refers to the current DAT block, and is a section identifier as described by the filesystem specification.

The length of the final block is used in read-only streams to determine when the end of the stream has been reached.
