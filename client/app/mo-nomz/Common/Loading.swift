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
}
