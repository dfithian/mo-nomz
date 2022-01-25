//
//  AddController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/16/22.
//

import UIKit

class AddController: UINavigationController, UINavigationControllerDelegate {
    var onChange: (() -> Void)?
    
    func navigationController(_ navigationController: UINavigationController, willShow viewController: UIViewController, animated: Bool) {
        if let vc = viewController as? RecipeAddController {
            vc.navigationVc = self
        }
        if let vc = viewController as? GroceryAddBlobController {
            vc.navigationVc = self
        }
        if let vc = viewController as? GroceryAddRecipeController {
            vc.navigationVc = self
        }
    }
    
    func switchToLink() {
        let storyboard = UIStoryboard(name: "AddItems", bundle: nil)
        let vc = storyboard.instantiateViewController(withIdentifier: "addLink") as! RecipeAddController
        setViewControllers([vc], animated: false)
    }
    
    func switchToManual() {
        let storyboard = UIStoryboard(name: "AddItems", bundle: nil)
        let vc = storyboard.instantiateViewController(withIdentifier: "addBlob") as! GroceryAddBlobController
        setViewControllers([vc], animated: false)
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        self.delegate = self
        switchToLink()
    }
}
