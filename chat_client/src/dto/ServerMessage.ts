import ServerMessageType from "./enums/ServerMessageType";
import ClientInfo from "./ClientInfo";

export type ServerMessage =
    | { type: ServerMessageType.ClientInfo; client: ClientInfo }
    | { type: ServerMessageType.Clients; clients: ClientInfo[] }
    | { type: ServerMessageType.Messages; messages: Array<{ id: string; username: string; message: string; datetime: string }> }
    | { type: ServerMessageType.SystemMessage; text: string }

