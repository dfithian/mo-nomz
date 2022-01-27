//
//  Loading.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/17/21.
//

import UIKit

extension UIViewController {
    func startLoading() -> UIActivityIndicatorView {
        let progress = UIActivityIndicatorView(style: .large)
        progress.translatesAutoresizingMaskIntoConstraints = true
        DispatchQueue.main.async {
            self.view.isUserInteractionEnabled = false
            progress.center = self.view.center
            progress.startAnimating()
            self.view.addSubview(progress)
        }
        return progress
    }
    
    func stopLoading(_ progress: UIActivityIndicatorView) {
        DispatchQueue.main.async {
            self.view.isUserInteractionEnabled = true
            progress.stopAnimating()
            progress.removeFromSuperview()
        }
    }
    
    func startProgress() -> UIProgressView {
        let progress = UIProgressView(progressViewStyle: .bar)
        progress.translatesAutoresizingMaskIntoConstraints = true
        DispatchQueue.main.async {
            self.view.isUserInteractionEnabled = false
            progress.trackTintColor = UIColor.lightGray
            progress.tintColor = UIColor.systemBlue
            progress.center = CGPoint(x: self.view.center.x, y: 2 * self.view.frame.size.height / 3)
            self.view.addSubview(progress)
        }
        return progress
    }
    
    func stopLoading(_ progress: UIProgressView) {
        DispatchQueue.main.async {
            self.view.isUserInteractionEnabled = true
            progress.removeFromSuperview()
        }
    }
}
