//
//  TableCells.swift
//  mo-nomz
//
//  Created by Dan Fithian on 2/3/22.
//

import UIKit

class OneButton: UITableViewCell {
    @IBOutlet weak var button: UIButton!
}

class OneLabel: UITableViewCell {
    @IBOutlet weak var label: UILabel!
}

class OneText: UITableViewCell {
    @IBOutlet weak var text_: UITextView!
}

class OneTextField: UITableViewCell {
    @IBOutlet weak var text_: UITextField!
}

class LabelButton: UITableViewCell {
    @IBOutlet weak var label: UILabel!
    @IBOutlet weak var button: UIButton!
}

class TwoLabelOneButton: UITableViewCell {
    @IBOutlet weak var oneLabel: UILabel!
    @IBOutlet weak var button: UIButton!
    @IBOutlet weak var twoLabel: UILabel!
}

class LabelSwitch: UITableViewCell {
    @IBOutlet weak var label: UILabel!
    @IBOutlet weak var switch_: UISwitch!
}
