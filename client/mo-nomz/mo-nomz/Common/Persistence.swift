//
//  Persistence.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import Foundation

struct State {
    let userId: Int
    let apiToken: String
}

class Persistence {
    static let account: Data = "mo-nomz.mo-nomz.user-data".data(using: .utf8)!
    static let server: Data = "mo-nomz.herokuapp.com".data(using: .utf8)!
    static func loadState() -> State? {
        let query = [
            kSecClass as String: kSecClassInternetPassword,
            kSecAttrAccount as String: account,
            kSecAttrServer as String: server,
            kSecReturnData as String: true
        ] as CFDictionary
        var item: CFTypeRef?
        let status = SecItemCopyMatching(query as CFDictionary, &item)
        guard status == errSecSuccess else { return nil }
        let userId = UserDefaults.standard.integer(forKey: "userId")
        guard userId > 0 else { return nil }
        return State(userId: userId, apiToken: String(data: item as! Data, encoding: .utf8)!)
    }
    static func clearState() {
        let query = [
            kSecClass as String: kSecClassInternetPassword,
            kSecAttrAccount as String: account,
            kSecAttrServer as String: server
        ] as CFDictionary
        SecItemDelete(query)
    }
    static func setState(_ state: State) {
        UserDefaults.standard.set(state.userId, forKey: "userId")
        let query = [
            kSecClass as String: kSecClassInternetPassword,
            kSecAttrAccount as String: account,
            kSecAttrServer as String: server,
            kSecValueData as String: state.apiToken.data(using: .utf8)!
        ] as CFDictionary
        var ref: AnyObject?
        SecItemAdd(query, &ref)
    }
}
