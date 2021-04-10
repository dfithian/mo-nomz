//
//  LoginController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit

class LoginController: UIViewController {
    @IBOutlet weak var username: UITextField!
    @IBOutlet weak var go: UIButton!
    @IBAction func didTapLogin(_ sender: Any) {
        guard let newUsername = username?.text! else { return () }
        Actions.loadUser(username: newUsername, completion: { [weak self] (resp) -> Void in
            Persistence.setState(state: State(username: newUsername, userId: resp.userId))
            self?.loadMain()
        })
    }
    
    func loadMain() {
        DispatchQueue.main.async {
            let sb = UIStoryboard(name: "Main", bundle: nil)
            let vc = sb.instantiateInitialViewController()!
            let scene = self.view.window?.windowScene?.delegate as! SceneDelegate
            scene.window?.rootViewController = vc
        }
    }
}
