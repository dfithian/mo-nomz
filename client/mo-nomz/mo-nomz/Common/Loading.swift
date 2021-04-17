//
//  Loading.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/17/21.
//

import UIKit

class SpinnerViewController: UIViewController {
    var spinner = UIActivityIndicatorView(style: .medium)

    override func loadView() {
        view = UIView()
        view.backgroundColor = UIColor(white: 0, alpha: 0.7)

        spinner.translatesAutoresizingMaskIntoConstraints = false
        spinner.startAnimating()
        view.addSubview(spinner)

        spinner.centerXAnchor.constraint(equalTo: view.centerXAnchor).isActive = true
        spinner.centerYAnchor.constraint(equalTo: view.centerYAnchor).isActive = true
    }
}

extension UIViewController {
    func startLoading() -> SpinnerViewController {
        let spinner = SpinnerViewController()
        DispatchQueue.main.async {
            self.addChild(spinner)
            spinner.view.frame = self.view.frame
            self.view.addSubview(spinner.view)
            spinner.didMove(toParent: self)
        }
        return spinner
    }
    
    func stopLoading(spinner: SpinnerViewController) {
        DispatchQueue.main.async {
            spinner.willMove(toParent: nil)
            spinner.view.removeFromSuperview()
            spinner.removeFromParent()
        }
    }
}
