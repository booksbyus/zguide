/**
*  Pub-Sub Tracing (Espresso Pattern) 
*  explained in
*  https://zguide.zeromq.org/docs/chapter5
*/

"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.runListenerThread = exports.runPubThread = exports.runSubThread = void 0;

const zmq = require("zeromq"),
      publisher = new zmq.Publisher,
      pubKeypair = zmq.curveKeyPair(),
      publicKey = pubKeypair.publicKey;

var interrupted = false;

function getRandomInt(max) {
    return Math.floor(Math.random() * Math.floor(max));
}

async function runSubThread() {

    const subscriber = new zmq.Subscriber;
    const subKeypair = zmq.curveKeyPair();
    
    // Setup encryption.
    for (const s of [subscriber]) {
        subscriber.curveServerKey = publicKey; // '03P+E+f4AU6bSTcuzvgX&oGnt&Or<rN)FYIPyjQW'
        subscriber.curveSecretKey = subKeypair.secretKey;
        subscriber.curvePublicKey = subKeypair.publicKey;
    }
    
    await subscriber.connect("tcp://127.0.0.1:6000");
    console.log('subscriber connected! subscribing A,B,C and D..');
    
    //subscribe all at once - simultaneous subscriptions needed
    Promise.all([
        subscriber.subscribe("A"),
        subscriber.subscribe("B"),
        subscriber.subscribe("C"),
        subscriber.subscribe("D"),
        subscriber.subscribe("E"),
    ]);
    for await (const [msg] of subscriber) {
        console.log(`Received at subscriber: ${msg}`);
        if (interrupted) {
            await subscriber.disconnect("tcp://127.0.0.1:6000");
            await subscriber.close();
            break;
        }
    }
}

//Run the Publisher Thread!
async function runPubThread() {

   // Setup encryption.
    for (const s of [publisher]) {
        s.curveServer = true;
        s.curvePublicKey = publicKey;
        s.curveSecretKey = pubKeypair.secretKey;
    }
    await publisher.bind("tcp://127.0.0.1:6000");
    console.log(`Started publisher at tcp://127.0.0.1:6000 ..`);
    var subs = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    while (!interrupted) { //until ctl+c 
        var str = `${subs.charAt(getRandomInt(10))}-${getRandomInt(100000).toString().padStart(6, '0')}`; //"%c-%05d";
        console.log(`Publishing ${str}`);
        if (-1 == await publisher.send(str))
            break; //Interrupted
        await new Promise(resolve => setTimeout(resolve, 1000));
    }
    //if(! publisher.closed())
    await publisher.close();
}

//Run the Pipe
async function runListenerThread() {
    //a pipe using 'Pair' which receives and transmits data
    const pipe = new zmq.Pair;
    await pipe.connect("tcp://127.0.0.1:6000");
    await pipe.bind("tcp://127.0.0.1:6001");
    
    console.log('starting pipe (using Pair)..');
    
    while (!interrupted) {
        await pipe.send(await pipe.receive());
    }

    setTimeout(() => {
        console.log('Terminating pipe..');
        pipe.close();
    }, 1000);
    
    //a pipe using 'Proxy' <= not working, but give it a try. 
    // Still working with Proxy
    /*
    const pipe = new zmq.Proxy (new zmq.Router, new zmq.Dealer)
    await pipe.backEnd.connect("tcp://127.0.0.1:6000")
    await pipe.frontEnd.bind("tcp://127.0.0.1:6001")    
    await pipe.run()
    setTimeout(() => {
        console.log('Terminating pipe..');
        await pipe.terminate()
     }, 10000);
     */
    
}

exports.runSubThread = runSubThread;
exports.runPubThread = runPubThread;
exports.runListenerThread = runListenerThread;

process.on('SIGINT', function () {
    interrupted = true;
});

process.setMaxListeners(30);

async function main() {
    //execute all at once
    Promise.all([
        runPubThread(),
        runListenerThread(),
        runSubThread(),
    ]);
}

main().catch(err => {
    console.error(err);
    process.exit(1);
});
