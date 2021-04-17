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
        return "http://localhost:8080/"
        #else
        return "https://mo-nomz.herokuapp.com/"
        #endif
    }
    
    static var supportUrl: String {
        return "https://monomz.wordpress.com"
    }
}
