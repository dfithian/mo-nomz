//
//  BannerController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/23/21.
//

import GoogleMobileAds
import UIKit

class BannerController: UIViewController {
    let banner: GADBannerView = {
        let b = GADBannerView()
        b.adUnitID = Configuration.googleBannerId
        b.load(GADRequest())
        b.backgroundColor = .secondarySystemBackground
        return b
    }()
    
    override func viewDidLoad() {
        banner.rootViewController = self
        view.addSubview(banner)
    }
    
    override func viewDidLayoutSubviews() {
        super.viewDidLayoutSubviews()
        banner.frame = CGRect(x: 0, y: 0, width: view.frame.size.width, height: view.frame.size.height)
    }
}
