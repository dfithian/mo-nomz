//
//  ProfileController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit

class ProfileController: UIViewController {
    var state: State? = nil
    @IBOutlet weak var username: UILabel!
    @IBAction func didTapLogout(_ sender: Any) {
        let ok = UIAlertAction(title: "OK", style: .default, handler: { [weak self] (action) -> Void in self?.doLogout() })
        let cancel = UIAlertAction(title: "Cancel", style: .cancel, handler: nil)
        let confirmation = UIAlertController(title: "Logout", message: "Are you sure you want to log out?", preferredStyle: .alert)
        confirmation.addAction(ok)
        confirmation.addAction(cancel)
        self.present(confirmation, animated: true, completion: nil)
    }
    @IBAction func didTapContactSupport(_ sender: Any) {
        if let url = URL(string: "mailto:\(Configuration.environment.contactEmail)") {
            if #available(iOS 10.0, *) {
                UIApplication.shared.open(url)
            } else {
                UIApplication.shared.openURL(url)
            }
        }
    }
    
    override func viewDidLoad() {
        state = Persistence.loadState()!
        username.text = "Hi, " + state!.username + "!"
    }
    
    func doLogout() {
        Persistence.clearState()
        DispatchQueue.main.async {
            let sb = UIStoryboard(name: "Login", bundle: nil)
            let vc = sb.instantiateInitialViewController()!
            let scene = self.view.window?.windowScene?.delegate as! SceneDelegate
            scene.window?.rootViewController = vc
        }
    }
}
