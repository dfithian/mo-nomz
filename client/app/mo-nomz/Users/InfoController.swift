//
//  InfoController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 5/9/21.
//

import UIKit

class InfoController: UIViewController {
    @IBOutlet weak var banner: UIView!
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? BannerController, segue.identifier == "embedBanner" {
            vc.height = banner.constraints.filter({ $0.identifier == "height" }).first
        }
    }
}
