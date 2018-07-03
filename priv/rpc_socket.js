const RPCSocket = function (host, port, path, encoder, decoder) {
    this.websocket = new WebSocket(`ws://${host}:${port}${path}`);
    this.websocket.binaryType = 'arraybuffer';
    this.waiting_messages = [];

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
    if (data[0] === true) {
        const message_id = data[1];
        if (this.waiting_messages[message_id]) {
            this.waiting_messages[message_id](data[2]);
        }
    } else if (data[0] === false) {
        const functions = this.client_functions;
        if (functions[data[1]]) {
            const f = functions[data[1]];
            const name = f.name;
            this.client[name].call(this, ...data[2]);
        }
    }
};

RPCSocket.prototype.execute = function (id, parameters) {
    return new Promise((resolve) => {
        let message_id = -1;
        do {
            message_id = Math.ceil(Math.random() * 100000);
        } while (this.waiting_messages[message_id]);

        this.waiting_messages[message_id] = resolve;
        const message = [id, message_id, parameters];
        this.send(message);
    });
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
