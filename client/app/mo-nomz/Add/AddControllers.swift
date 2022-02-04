//
//  AddControllers.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/16/22.
//

import UIKit

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

enum AddType {
    case link
    case manual
    case photo
}

class AddDetailController: SimpleController {
    @IBOutlet weak var switcher: UIBarButtonItem!
    
    var navigationVc: AddController? = nil
    
    func addType() -> AddType {
        preconditionFailure("Must override addType()")
    }

    private func setupSwitcher() {
        let f = { (type: AddType) -> UIMenuElement.State in
            self.addType() == type ? .on : .off
        }
        switcher.menu = UIMenu(options: .displayInline, children: [
            UIAction(title: "Add link", image: UIImage(systemName: "link"), state: f(.link), handler: { _ in self.navigationVc?.switchToLink() }),
            UIAction(title: "Add manual", image: UIImage(systemName: "pencil"), state: f(.manual), handler: { _ in self.navigationVc?.switchToManual() }),
            UIAction(title: "Add photos", image: UIImage(systemName: "photo.on.rectangle"), state: f(.photo), handler: { _ in self.navigationVc?.switchToPhoto() })
        ])
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        setupSwitcher()
    }
}
