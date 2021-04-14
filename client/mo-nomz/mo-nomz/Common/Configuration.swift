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
    
    var baseURL: String {
        switch self {
        case .Debug: return "http://localhost:8080/"
        case .Release: return "https://mo-nomz.herokuapp.com/"
        }
    }
    
    var contactEmail: String {
        switch self {
        case .Debug: return "daniel.m.fithian@gmail.com"
        case .Release: return "daniel.m.fithian@gmail.com"
        }
    }
}

struct Configuration {
    static var environment: Environment = {
        if let rawValue = ProcessInfo.processInfo.environment["CONFIGURATION"] {
            return rawValue == "release" ? Environment.Release : Environment.Debug
        }
        return Environment.Debug
    }()
}
