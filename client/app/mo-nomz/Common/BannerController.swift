//
//  BannerController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/23/21.
//

import GoogleMobileAds
import UIKit

class BannerController: UIViewController, GADBannerViewDelegate {
    var banner: GADBannerView?
    var height: NSLayoutConstraint?
    
    private func removeBanner() {
        banner?.removeFromSuperview()
        height?.constant = 0
        parent?.updateViewConstraints()
    }
    
    func bannerView(_ bannerView: GADBannerView, didFailToReceiveAdWithError error: Error) {
        print(error)
        removeBanner()
    }
    
    override func reloadInputViews() {
        super.reloadInputViews()
        if User.purchased(.removeAds) {
            removeBanner()
        }
    }
    
    override func viewDidLayoutSubviews() {
        super.viewDidLayoutSubviews()
        if User.purchased(.removeAds) {
            removeBanner()
            return
        }
        banner?.frame = CGRect(x: 0, y: 0, width: view.frame.size.width, height: view.frame.size.height)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        if User.purchased(.removeAds) {
            removeBanner()
        }
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        if User.purchased(.removeAds) {
            removeBanner()
            return
        }
        banner = GADBannerView()
        banner?.delegate = self
        banner?.adUnitID = Configuration.googleBannerId
        banner?.load(GADRequest())
        banner?.backgroundColor = .secondarySystemBackground
        banner?.rootViewController = self
        banner?.translatesAutoresizingMaskIntoConstraints = false
        view.addSubview(banner!)
    }
}
