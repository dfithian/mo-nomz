//
//  TabBarController.swift
//  
//
//  Created by Dan Fithian on 11/9/22.
//

import UIKit

class TabBarController: UITabBarController, UITabBarControllerDelegate {
    let GROCERIES = 0
    let RECIPES = 1
    let INFO = 2

    override func tabBar(_ tabBar: UITabBar, didSelect item: UITabBarItem) {
        guard let index = tabBar.selectedItem?.tag else { return }
        switch index {
        case GROCERIES:
            (viewControllers?[GROCERIES] as? GroceryController)?.reloadData()
            break
        case RECIPES:
            (viewControllers?[RECIPES] as? RecipeController)?.reloadData()
            break
        default: break
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        delegate = self
    }
}
