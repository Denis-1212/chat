import { useState } from 'react';
import Controllers from '../dto/enums/Controllers';
import Actions from '../dto/enums/Actions';
import { Payload } from '../dto/Payload';

export function useWebSocket() {
    const [connected, setConnected] = useState(false);
    const [socket, setSocket] = useState<WebSocket | null>(null);

    const connect = (username: string) => {
        if (socket?.readyState === WebSocket.OPEN) return;

        // ws.current = new WebSocket(`ws://${window.location.host}/websocket`);
        const ws = new WebSocket(`ws://localhost:8080/websocket`);
        setSocket(ws);

        ws.onopen = () => {
            setConnected(true);
            if (ws?.readyState === WebSocket.OPEN) {
                const msg = { controller: Controllers.UserController, action: Actions.Login, payload: { username } }
                ws.send(JSON.stringify(msg));
            }
        };


        ws.onclose = () => {
            setConnected(false);
            setSocket(null);
        };

        ws.onerror = () => {
            setConnected(false);
        };
    };

    function sendMessage<C extends keyof Payload, A extends keyof Payload[C]>(
        controller: C,
        action: A,
        payload: Payload[C][A]
    ) {
        if (socket?.readyState === WebSocket.OPEN) {
            socket.send(JSON.stringify({ controller, action, payload }));
        }
    }

    const disconnect = () => {
        socket?.close();
    };


    return {
        connected,
        connect,
        disconnect,
        sendMessage,
        socket: socket,
    };
}
