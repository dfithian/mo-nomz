//
//  ProfileController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit

class ProfileController: UITableViewController {
    @IBAction func didTapContactSupport(_ sender: Any) {
        if let url = URL(string: Configuration.supportUrl) {
            if #available(iOS 10.0, *) {
                UIApplication.shared.open(url)
            } else {
                UIApplication.shared.openURL(url)
            }
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        tableView.separatorStyle = .none
        tableView.layer.cornerRadius = 5
    }
}
