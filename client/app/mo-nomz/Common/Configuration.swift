//
//  Configuration.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/11/21.
//

import Foundation

class Configuration {
    static var baseURL: String {
        return "https://app.mo-nomz.com/"
    }
    
    static var helpUrl: String {
        return "https://app.mo-nomz.com/#/support"
    }
    
    static var accountKey: String {
        return "mo-nomz.mo-nomz.user-data"
    }
    
    static var serverKey: String {
        return "mo-nomz.herokuapp.com"
    }
    
    static var googleBannerId: String {
        return "ca-app-pub-1682263410056980/9786643840"
    }
    
    static var venmoUrl: String {
        return "https://venmo.com/code?user_id=1305125788319744156"
    }
    
    static var allDomains: [String] {
        return [
            "app.mo-nomz.com",
            "mo-nomz.herokuapp.com",
            "mo-nomz-dev.herokuapp.com"
        ]
    }
    
    static var isDebug: Bool {
        #if targetEnvironment(simulator)
        return true
        #else
        return false
        #endif
    }
}
