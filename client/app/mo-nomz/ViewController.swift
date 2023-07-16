//
//  ViewController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 6/18/23.
//

import UIKit

class ViewController: UIViewController {
    @IBOutlet weak var banner: UIView!
    @IBOutlet weak var bannerHeight: NSLayoutConstraint!
    var tab: TabBarController!
    private var selected: Int = User.preference(.mealsDefaultTab) ? 1 : 0
    
    func setSelected(index: Int) {
        selected = index
        tab?.selectedIndex = selected
    }
    
    func reloadData() {
        tab?.reloadData()
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? BannerController, segue.identifier == "banner" {
            vc.height = bannerHeight
        }
        if let vc = segue.destination as? TabBarController, segue.identifier == "main" {
            vc.selectedIndex = selected
            tab = vc
        }
    }
}
