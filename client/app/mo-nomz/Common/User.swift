//
//  User.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import Foundation

struct State {
    let userId: Int
    let apiToken: String
}

class User {
    static let account: Data = Configuration.accountKey.data(using: .utf8)!
    static let server: Data = Configuration.serverKey.data(using: .utf8)!
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
        let userId = UserDefaults.shared.integer(forKey: "userId")
        guard userId > 0 else { return nil }
        let apiToken = String(data: item as! Data, encoding: .utf8)!
        return State(userId: userId, apiToken: apiToken)
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
        UserDefaults.shared.set(state.userId, forKey: "userId")
        let query = [
            kSecClass as String: kSecClassInternetPassword,
            kSecAttrAccount as String: account,
            kSecAttrServer as String: server,
            kSecValueData as String: state.apiToken.data(using: .utf8)!
        ] as CFDictionary
        var ref: AnyObject?
        SecItemAdd(query, &ref)
    }
    static func exported() -> Bool {
        return UserDefaults.shared.bool(forKey: "exported")
    }
    static func setDidExport() {
        UserDefaults.shared.set(true, forKey: "exported")
    }
    static func stepsPulled() -> Bool {
        return UserDefaults.shared.bool(forKey: "pulledSteps")
    }
    static func setDidPullSteps() {
        UserDefaults.shared.set(true, forKey: "pulledSteps")
    }
    static func stepsCleaned() -> Bool {
        return UserDefaults.shared.bool(forKey: "cleanedSteps")
    }
    static func setDidCleanSteps() {
        UserDefaults.shared.set(true, forKey: "cleanedSteps")
    }
    static func groupsInitialized() -> Bool {
        return UserDefaults.shared.bool(forKey: "groupsInitialized")
    }
    static func setDidInitializeGroups() {
         UserDefaults.shared.set(true, forKey: "groupsInitialized")
    }
    static func dismissedReorderMergeTip() -> Bool {
        return UserDefaults.shared.bool(forKey: "dismissedReorderMergeTip") || preference(.noTips)
    }
    static func setDidDismissReorderMergeTip() {
        UserDefaults.shared.set(true, forKey: "dismissedReorderMergeTip")
    }
    static func dismissedIngredientMergeTip() -> Bool {
        return UserDefaults.shared.bool(forKey: "dismissedIngredientMergeTip") || preference(.noTips)
    }
    static func setDidDismissIngredientMergeTip() {
        UserDefaults.shared.set(true, forKey: "dismissedIngredientMergeTip")
    }
    static func dismissedMergeWarning() -> Bool {
        return UserDefaults.shared.bool(forKey: "dismissedMergeWarning")
    }
    static func setDidDismissMergeWarning() {
        UserDefaults.shared.set(true, forKey: "dismissedMergeWarning")
    }
    static func dismissedIngredientMergeWarning() -> Bool {
        return UserDefaults.shared.bool(forKey: "dismissedIngredientMergeWarning")
    }
    static func setDidDismissIngredientMergeWarning() {
        UserDefaults.shared.set(true, forKey: "dismissedIngredientMergeWarning")
    }
    static func dismissedStepReorderTip() -> Bool {
        return UserDefaults.shared.bool(forKey: "dismissedStepReorderTip") || preference(.noTips)
    }
    static func setDidDismissStepReorderTip() {
        UserDefaults.shared.set(true, forKey: "dismissedStepReorderTip")
    }
    static func purchased(_ x: ProductRole) -> Bool {
        return UserDefaults.shared.bool(forKey: x.productIdentifier)
    }
    static func setDidPurchase(_ x: ProductRole) {
        UserDefaults.shared.set(true, forKey: x.productIdentifier)
    }
    static func preference(_ key: PreferenceRole) -> Bool {
        return UserDefaults.shared.bool(forKey: key.identifier)
    }
    static func setPreference(_ key: PreferenceRole, value: Bool) {
        UserDefaults.shared.set(value, forKey: key.identifier)
    }
}
