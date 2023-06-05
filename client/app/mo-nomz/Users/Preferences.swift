//
//  Preferences.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/21/22.
//

import Foundation

enum PreferenceRole {
    case mealsDefaultTab

    var description: String {
        switch self {
        case .mealsDefaultTab: return "Use Meals as default tab"
        }
    }

    var identifier: String {
        switch self {
        case .mealsDefaultTab: return "meals-default-tab"
        }
    }
}
