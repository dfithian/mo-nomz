//
//  AddController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/16/22.
//

import UIKit

class AddController: UIViewController {
    @IBOutlet weak var back: UIButton!
    @IBOutlet weak var submit: UIBarButtonItem!
    @IBOutlet weak var instead: UIBarButtonItem!

    var insteadSwitch: Bool = true
    var onChange: (() -> Void)? = nil
    var navigationVc: AddNavigationController!
    
    @IBAction func didTapCancel(_ sender: Any?) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    @IBAction func didTapInstead(_ sender: Any?) {
        insteadSwitch ? navigationVc.switchToManual() : navigationVc.switchToLink()
        insteadSwitch = !insteadSwitch
    }
    
    @IBAction func didTapSave(_ sender: Any?) {
        navigationVc.currentVc.submit(onChange)
    }
    
    @IBAction func didTapBack(_ sender: Any?) {
        navigationVc.popViewController(animated: true)
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddNavigationController, segue.identifier == "embedAdd" {
            navigationVc = vc
            vc.back = back
            vc.submit = submit
            vc.instead = instead
        }
    }
}

class AddNavigationController: UINavigationController, UINavigationControllerDelegate {
    var currentVc: Submit!
    var back: UIButton!
    var submit: UIBarButtonItem!
    var instead: UIBarButtonItem!
    
    func navigationController(_ navigationController: UINavigationController, willShow viewController: UIViewController, animated: Bool) {
        back.alpha = 0
        if let vc = viewController as? RecipeAddController {
            currentVc = vc
        }
        if let vc = viewController as? GroceryAddBlobController {
            currentVc = vc
            vc.onIsRecipe = { self.submit.title = $0 ? "Next" : "Add to list" }
        }
        if let vc = viewController as? GroceryAddRecipeController {
            currentVc = vc
            back.alpha = 1
        }
    }
    
    func switchToLink() {
        let storyboard = UIStoryboard(name: "AddItems", bundle: nil)
        let vc = storyboard.instantiateViewController(withIdentifier: "addLink") as! RecipeAddController
        submit.title = "Add to list"
        instead.title = "Add manual"
        setViewControllers([vc], animated: false)
    }
    
    func switchToManual() {
        let storyboard = UIStoryboard(name: "AddItems", bundle: nil)
        let vc = storyboard.instantiateViewController(withIdentifier: "addBlob") as! GroceryAddBlobController
        submit.title = "Add to list"
        instead.title = "Add link"
        setViewControllers([vc], animated: false)
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        self.delegate = self
        switchToLink()
    }
}
