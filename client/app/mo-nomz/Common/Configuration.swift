//
//  Configuration.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/11/21.
//

import Foundation

enum Environment: String {
    case Debug = "debug"
    case Release = "release"
}

class Configuration {
    static var environment: Environment {
        #if DEBUG
        return Environment.Debug
        #else
        return Environment.Release
        #endif
    }
    
    static var baseURL: String {
        #if DEBUG
        return "http://mo-nomz-dev.herokuapp.com/"
        #else
        return "https://mo-nomz.herokuapp.com/"
        #endif
    }
    
    static var helpUrl: String {
        return "https://mo-nomz.herokuapp.com/#how-it-works"
    }
    
    static var accountKey: String {
        #if DEBUG
        return "mo-nomz.mo-nomz.user-data-dev"
        #else
        return "mo-nomz.mo-nomz.user-data"
        #endif
    }
    
    static var serverKey: String {
        #if DEBUG
        return "mo-nomz-dev.herokuapp.com"
        #else
        return "mo-nomz.herokuapp.com"
        #endif
    }
    
    static var googleBannerId: String {
        #if DEBUG
        return "ca-app-pub-3940256099942544/2934735716"
        #else
        return "ca-app-pub-1682263410056980/9786643840" // "ca-app-pub-3940256099942544/2934735716"
        #endif
    }
    
    static var venmoUrl: String {
        return "https://venmo.com/code?user_id=1305125788319744156"
    }
}
