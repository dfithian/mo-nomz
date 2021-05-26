//
//  Toolbar.swift
//  mo-nomz
//
//  Created by Dan Fithian on 5/26/21.
//

import UIKit

class Toolbar: UIToolbar {
    override var intrinsicContentSize: CGSize {
        var x = super.intrinsicContentSize
        x.height = 50
        return x
    }
}
