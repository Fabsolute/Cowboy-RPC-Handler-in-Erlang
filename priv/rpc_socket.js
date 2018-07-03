const RPCSocket = function (host, port, path, encoder, decoder) {
    this.websocket = new WebSocket(`ws://${host}:${port}${path}`);
    this.websocket.binaryType = 'arraybuffer';

    this.websocket.onopen = (event) => {
        this.onOpen(event);
    };

    this.websocket.onclose = (event) => {
        this.onClose(event)
    };

    this.websocket.onmessage = (event) => {
        this.onMessage(event);
    };

    this.websocket.onerror = (event) => {
        this.onError(event)
    };

    const functions = this.getFunctions();
    if (functions) {
        this.server = functions.server;
        this.client_functions = functions.client_functions;
        this.encoder = encoder;
        this.decoder = decoder;
    } else {
        throw(new Error("server js file not loaded"));
    }
};

RPCSocket.prototype.client = {};

RPCSocket.prototype.onOpen = function (event) {
};

RPCSocket.prototype.onMessage = function (event) {
    const data = this.decoder(event.data);
    const functions = this.client_functions;
    if (functions[data[0]]) {
        const f = functions[data[0]];
        const name = f.name;
        this.client[name].call(this, ...data[1]);
    }
};

RPCSocket.prototype.execute = function (id, parameters) {
    const message = [id, parameters];
    this.send(message);
};

RPCSocket.prototype.send = function (message) {
    if (this.websocket.readyState === this.websocket.OPEN) {
        const data = this.encoder(message);
        this.websocket.send(data);
    }
};

RPCSocket.prototype.onClose = function (event) {
};

RPCSocket.prototype.onError = function (event) {
};

RPCSocket.prototype.getFunctions = function () {
};
