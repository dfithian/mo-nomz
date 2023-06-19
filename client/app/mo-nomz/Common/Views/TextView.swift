//
//  TextView.swift
//  mo-nomz
//
//  Created by Dan Fithian on 6/18/23.
//

import UIKit

class TextView: UITextView {
    required init?(coder: NSCoder) {
        super.init(coder: coder)
        self.layer.backgroundColor = self.traitCollection.userInterfaceStyle == .dark ? UIColor.black.cgColor : UIColor.secondarySystemBackground.cgColor
    }
}
