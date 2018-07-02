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
    this.server = functions.server;
    this.client_functions = functions.client_functions;
    this.encoder = encoder;
    this.decoder = decoder;
};

RPCSocket.prototype.client = {};

RPCSocket.prototype.onOpen = function (event) {
};

RPCSocket.prototype.onMessage = function (event) {
    const data = this.decoder(event.data);
    const functions = this.client_functions;
    if (functions[data.m]) {
        const f = functions[data.m];
        const name = f.name;
        this.client[name].call(this, ...data.p);
    }
};

RPCSocket.prototype.execute = function (id, parameters) {
    const message = {
        m: id,
        p: parameters
    };
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
