export type WebSocketMessage =
    | { type: 'client_init'; username: string }
    | { type: 'message'; text: string; from?: string }
    | { type: 'system'; text: string }
    | { type: 'client_id'; id: string }
    | { type: 'client_list'; clients: { id: string; name: string }[] };

type MessageHandler = (data: WebSocketMessage) => void;
type ErrorHandler = (err: Event) => void;
type CloseHandler = () => void;

export class WebSocketService {
    private socket: WebSocket | null = null;

    constructor(
        private readonly url: string,
        private readonly onMessage: MessageHandler,
        private readonly onClose?: CloseHandler,
        private readonly onError?: ErrorHandler
    ) { }

    connect(username: string) {
        this.socket = new WebSocket(this.url);

        this.socket.onopen = () => {
            this.send({ type: 'client_init', username });
        };

        this.socket.onmessage = (event) => {
            try {
                const data: WebSocketMessage = JSON.parse(event.data);
                this.onMessage(data);
            } catch {
                // Фоллбек на текст
                if (event.data.startsWith('ID:')) {
                    this.onMessage({ type: 'system', text: event.data });
                } else {
                    this.onMessage({ type: 'system', text: `Raw: ${event.data}` });
                }
            }
        };

        this.socket.onerror = (event) => {
            this.onError?.(event);
        };

        this.socket.onclose = () => {
            this.onClose?.();
        };
    }

    send(message: WebSocketMessage) {
        if (this.socket && this.socket.readyState === WebSocket.OPEN) {
            this.socket.send(JSON.stringify(message));
        } else {
            console.warn('WebSocket is not open');
        }
    }

    disconnect() {
        this.socket?.close();
    }

    isConnected() {
        return this.socket?.readyState === WebSocket.OPEN;
    }
}
