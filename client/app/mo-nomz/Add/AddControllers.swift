//
//  AddControllers.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/16/22.
//

import UIKit

enum AddType {
    case link
    case manualRecipes
    case manualGroceries
    case photo
    case manualPhoto
}

enum ScrapeType {
    case ingredient
    case step
}

struct ScrapeImageInfo {
    var image: UIImage
    var value: String
    let type: ScrapeType
}

struct ScrapeTextInfo {
    var value: String
    let type: ScrapeType
}

struct Scrape<A> {
    var ingredients: [A]
    var steps: [A]
}

class AddController: UINavigationController, UINavigationControllerDelegate {
    var onChange: (() -> Void)?
    var addType: AddType = .link

    func navigationController(_ navigationController: UINavigationController, willShow viewController: UIViewController, animated: Bool) {
        if let vc = viewController as? AddGroceryController {
            vc.navigationVc = self
        }
        if let vc = viewController as? AddLinkController {
            vc.navigationVc = self
        }
        if let vc = viewController as? AddManualController {
            vc.navigationVc = self
        }
        if let vc = viewController as? AddRecipePhotoController {
            vc.navigationVc = self
        }
        if let vc = viewController as? AddGroceryPhotoController {
            vc.navigationVc = self
        }
        if let vc = viewController as? ReviewPhotoController {
            vc.navigationVc = self
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        self.delegate = self
        let vc: UIViewController
        switch addType {
        case .link:
            vc = storyboard!.instantiateViewController(withIdentifier: "addLink")
            break
        case .manualRecipes:
            vc = storyboard!.instantiateViewController(withIdentifier: "addManual")
            (vc as! AddManualController).change = .manual
            break
        case .manualGroceries:
            vc = storyboard!.instantiateViewController(withIdentifier: "addGrocery")
            break
        case .photo:
            vc = storyboard!.instantiateViewController(withIdentifier: "addPhoto")
            break
        case .manualPhoto:
            vc = storyboard!.instantiateViewController(withIdentifier: "addGroceryPhoto")
            break
        }
        setViewControllers([vc], animated: false)
    }
}

class AddDetailController: UIViewController {
    var navigationVc: AddController? = nil
}
