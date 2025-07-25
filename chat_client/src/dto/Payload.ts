import Actions from './enums/Actions';
import Controllers from './enums/Controllers';

export type Payload = {
    [Controllers.MessageController]: {
        [Actions.SendMessage]: { content: string; datetime: string; userid?: string; username?: string };
    };
    [Controllers.UserController]: {
        [Actions.Login]: { username: string; };
    };
    [Controllers.SignalingController]: {
        [Actions.EndCall]: { type: string, to: string };
        [Actions.Answer]: { type: string, to: string, candidate: RTCIceCandidate } | { type: string, to: string, sdp: RTCSessionDescriptionInit };
        [Actions.Offer]: { type: string, to: string, candidate: RTCIceCandidate } | { type: string, to: string, sdp: RTCSessionDescriptionInit };
        [Actions.IceCandidate]: { type: string, to: string, candidate: RTCIceCandidate } | { type: string, to: string, sdp: RTCSessionDescriptionInit };
    };

};