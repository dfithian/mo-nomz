//
//  URL.swift
//  mo-nomz
//
//  Created by Dan Fithian on 2/23/22.
//

import UIKit

extension URL {
    func toRecipeUrl() -> URL? {
        let prefix = "/recipe?recipe_url="
        guard let base = URLComponents(url: self, resolvingAgainstBaseURL: true),
              let host = base.host, Configuration.allDomains.contains(host), base.fragment?.starts(with: prefix) ?? false,
              let link = base.fragment?.dropFirst(prefix.count),
              let recipeUrl = URL(string: String(link)), UIApplication.shared.canOpenURL(recipeUrl) else { return nil }
        return recipeUrl
    }
}
