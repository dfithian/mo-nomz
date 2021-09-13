//
//  ProfileController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit

class ProfileController: UITableViewController {    
    @IBAction func didTapNeedHelp(_ sender: Any) {
        needHelp()
    }
    
    @IBAction func didTapDonate(_ sender: Any) {
        if let url = URL(string: Configuration.venmoUrl) {
            if #available(iOS 10.0, *) {
                UIApplication.shared.open(url)
            } else {
                UIApplication.shared.openURL(url)
            }
        }
    }
}
