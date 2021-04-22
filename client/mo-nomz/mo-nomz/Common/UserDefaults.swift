//
//  UserDefaults.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/22/21.
//

import Foundation

extension UserDefaults {
    static var shared: UserDefaults {
        return UserDefaults(suiteName: "group.mo-nomz")!
    }
}
