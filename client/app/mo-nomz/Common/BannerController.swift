//
//  BannerController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/23/21.
//

import GoogleMobileAds
import UIKit

class BannerController: UIViewController, GADBannerViewDelegate {
    var banner: GADBannerView!
    var height: NSLayoutConstraint?
    
    func bannerView(_ bannerView: GADBannerView, didFailToReceiveAdWithError error: Error) {
        print("no")
        print(error)
        banner.removeFromSuperview()
        height?.constant = 0
        parent?.updateViewConstraints()
    }
    
    override func viewDidLayoutSubviews() {
        super.viewDidLayoutSubviews()
        banner.frame = CGRect(x: 0, y: 0, width: view.frame.size.width, height: view.frame.size.height)
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        banner = GADBannerView()
        banner.delegate = self
        banner.adUnitID = Configuration.googleBannerId
        banner.load(GADRequest())
        banner.backgroundColor = .secondarySystemBackground
        banner.rootViewController = self
        banner.translatesAutoresizingMaskIntoConstraints = false
        view.addSubview(banner)
    }
}
