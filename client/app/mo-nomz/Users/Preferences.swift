//
//  Preferences.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/21/22.
//

import Foundation

enum PreferenceRole {
    case mealsDefaultTab
    case showAds

    var description: String {
        switch self {
        case .mealsDefaultTab: return "Use Meals as default tab"
        case .showAds: return "Show Ads"
        }
    }

    var identifier: String {
        switch self {
        case .mealsDefaultTab: return "meals-default-tab"
        case .showAds: return "show-ads"
        }
    }
}
