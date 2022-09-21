# DOSP Project 1: Bitcoin Mining - Erlang's Actor Model
****
## Group Members
<ol>
    <li>Sai Ram Varma Budharaju (UFID: 3337-4276 | <a>sbudharaju@ufl.edu</a>)</li>
    <li>Pradyumna Pasumarty (UFID: 6907-9707 | <a>ppasumarty@ufl.edu</a>)</li>
</ol>

****

## Application Description

The main purpose of this application is to mine bitcoins whose SHA 256 Hashes start with a certain number of leading zeroes. 
This number can be specified in the input to the application. 
The application was built in Erlang in a Distributed paradigm using Erlang's Actor Model. 
Furthermore, a 'Client Server' architecture was employed to simulate the realistic usage of this application.

****

## Design Details

<ul>
    <li>
        Client Server Architecture
        <ul>
            <li>Both the client & server are initialized as nodes on remote systems</li>
            <li>These remote nodes are connected through erlang's Network Administration capabilities</li>
            <li>Post successful connection,the client triggers the server to start mining</li>
            <li>The required number of leading zeroes are also provided in the request</li>
        </ul>
    </li>
    <br/>
    <li>
        Actor Model Implementation
        <ul>
            <li>The server spawns a Boss & Child Actors to address the required task</li>
            <li>The Child Actors communicate with the Boss Actor through messages whenever valid coins are mined</li>
            <li>This Distributed implementation results in significant performance boost over a synchronous implementation</li>
        </ul>
    </li>
    <br/>
    <li>
        Hash Generation and Checking
        <ul>
            <li>The input key is created by concatenating a random string prefixed with a group member ID</li>
            <li>The key is then Hashed to SHA 256 standard using Erlang's Crypto functions</li>
            <li>If the hash has the required number of leading zeroes, it is considered as a valid bitcoin</li>
            <li>Such valid bitcoins are duly outputted onto the console</li>
        </ul>
    </li>
    <br/>
    <li>
        Performance Monitoring
        <ul>
            <li>The Run times and CPU Times are captured per actor & then aggregated</li>
            <li>This approach is employed to gain a detailed inference</li>
            <li>Furthermore, these values are written to a text file for plotting</li>
        </ul>
    </li>
</ul>

****

## Setup and Execution

<ol>
    <li>
        Setup
        <ul>
            <li>Client: <code>erl -name myclient@10.xx.xxx.xx -setcookie bitcoinminer</code></li>
            <li>Server: <code>erl -name myserver@10.xx.xxx.xx -setcookie bitcoinminer</code></li>
        </ul>
        <i>10.xx.xxx.xx indicates the IPv4 address of the respective machines</i>
    </li>
    <br/>
    <li>
        Connection
        <ul>
            <li>Client: <code>net_adm:ping('myserver@10.xx.xxx.xx').</code></li>
            <li>Upon successful connection, server returns <strong>pong</strong> to the client</li>
        </ul>
    </li>
    <br/>
    <li>
        Execution
        <ul>
            <li>Client Compilation: <code>c(miningclient).</code></li>
            <li>Server Compilation: <code>c(miningserver).</code></li>
            <li>Server Start: <code>miningserver:start_link().</code></li>
            <li>Client Call: <code>miningserver:triggerMiningOnServer(<<i>Number of leading zeroes</i>>).</code></li>
        </ul>
    </li>
</ol>

****

## Testing Methodology

<ol>
    <li>The application was initially tested for mining 400 coins synchronously i.e. without any actors</li>
    <li>The performance was measured for this execution for 4 leading zeroes</li>
    <li>The actor model was then implemented and the application was re-tested with the same parameters</li>
    <li>A significant performance improvement is observed when the Actor Model is implemented</li>
</ol>
<strong>Machine Specifications</strong>
<table>
    <th>Type</th>
    <th>Operating System</th>
    <th>Cores</th>
    <tr>
        <td>Server</td>
        <td>MacOS</td>
        <td>8</td>
    </tr>
    <tr>
        <td>Client</td>
        <td>Windows</td>
        <td>4</td>
    </tr>
</table>


****

## Testing Results

### Size of Work Unit for Best Performance = 4 Actors
This inference can be derived from the below table based on the CPU Time:Run Time Metric.

### Results (No. of Leading Zeroes = 4)

<table>
    <th>Number of Actors</th>
    <th>CPU Time (milliseconds)</th>
    <th>Run Time (milliseconds)</th>    
    <th>CPU Time : Run Time</th>
    <tr>
        <td>1 (Synchronous)</td>
        <td>102794</td>
        <td>102807</td>
        <td>1 : 1</td>
    </tr>
    <tr>
        <td>2</td>
        <td>58063</td>
        <td>111672</td>
        <td>1 : 1.9</td>
    </tr>
    <tr>
        <td>4</td>
        <td>30997</td>
        <td>114661</td>
        <td>1 : 3.7</td>
    </tr>
    <tr>
        <td>8</td>
        <td>61738</td>
        <td>138588</td>
        <td>1 : 2.2</td>
    </tr>
    <tr>
        <td>16</td>
        <td>61648</td>
        <td>130059</td>
        <td>1 : 2.1</td>
    </tr>
</table>

### Important Coin Details

<ul>
    Leading Zeroes: 8
    <ul>
        <li>ppasumarty;q/FNgETM    00000000cdd88ee1821c4f97a51f82213b17c64e1962cea7562399439bba4204</li>
    </ul>
    <br/>
    Leading Zeroes: 7
    <ul>
        <li>ppasumarty;a0f1N2J/    0000000b86b5ef0ba85669db8b43233b01da398e4f51b787dc1833f7e2ecbd02</li>
        <li>ppasumarty;7zd2xiel    00000001e2841e0fe2814642192abe98bfa7fb8b24d76bc5db13be1aa38da48d</li>
    </ul>
</ul>





