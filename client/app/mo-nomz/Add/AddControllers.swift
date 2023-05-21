//
//  AddControllers.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/16/22.
//

import UIKit

enum AddType {
    case link
    case manual
    case photo
    
    var title: String {
        switch self {
        case .link: return "Add link"
        case .manual: return "Add manual"
        case .photo: return "Add photos"
        }
    }
    
    var imageName: String {
        switch self {
        case .link: return "link"
        case .manual: return "pencil"
        case .photo: return "photo.on.rectangle"
        }
    }
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

    func navigationController(_ navigationController: UINavigationController, willShow viewController: UIViewController, animated: Bool) {
        if let vc = viewController as? AddLinkController {
            vc.navigationVc = self
        }
        if let vc = viewController as? AddManualController {
            vc.navigationVc = self
        }
        if let vc = viewController as? AddPhotoController {
            vc.navigationVc = self
        }
        if let vc = viewController as? ReviewPhotoController {
            vc.navigationVc = self
        }
    }
    
    func switchToLink() {
        let storyboard = UIStoryboard(name: "AddItems", bundle: nil)
        let vc = storyboard.instantiateViewController(withIdentifier: "addLink") as! AddLinkController
        setViewControllers([vc], animated: false)
    }
    
    func switchToManual() {
        let storyboard = UIStoryboard(name: "AddItems", bundle: nil)
        let vc = storyboard.instantiateViewController(withIdentifier: "addManual") as! AddManualController
        setViewControllers([vc], animated: false)
    }
    
    func switchToPhoto() {
        let storyboard = UIStoryboard(name: "AddItems", bundle: nil)
        let vc = storyboard.instantiateViewController(withIdentifier: "addPhoto") as! AddPhotoController
        setViewControllers([vc], animated: false)
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        self.delegate = self
        switchToLink()
    }
}

class AddDetailController: UIViewController {
    var navigationVc: AddController? = nil
}
