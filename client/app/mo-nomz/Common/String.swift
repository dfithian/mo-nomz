//
//  String.swift
//  mo-nomz
//
//  Created by Dan Fithian on 9/12/21.
//

import Foundation

extension String {
    func nonEmpty() -> String? {
        return self == "" ? nil : self
    }
}
