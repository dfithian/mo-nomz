//
//  Persistence.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import Foundation

struct State {
    let username: String
    let userId: Int
}

class Persistence {
    static func loadState() -> State? {
        if let username = UserDefaults.standard.string(forKey: "username") {
            let userId = UserDefaults.standard.integer(forKey: "userId")
            let state = State(username: username, userId: userId)
            return state
        }
        return nil
    }
    static func clearState() {
        UserDefaults.standard.removeObject(forKey: "username")
        UserDefaults.standard.removeObject(forKey: "userId")
    }
    static func setState(state: State) {
        UserDefaults.standard.set(state.username, forKey: "username")
        UserDefaults.standard.set(state.userId, forKey: "userId")
    }
}
