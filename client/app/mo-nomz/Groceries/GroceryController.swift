//
//  GroceryController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import GoogleMobileAds
import UIKit

class GroceryController: UIViewController {
    @IBOutlet weak var add: UIButton!
    @IBOutlet weak var hamburger: UIButton!
    @IBOutlet weak var count: UILabel!

    var groceryVc: GroceryListController? = nil
    var onChange: (() -> Void)? = nil

    func reloadData() {
        groceryVc?.reloadData()
        onChange?()
    }
    
    @IBAction func didTapClear(_ sender: Any?) {
        if !Database.selectGroceries().isEmpty {
            let handler = { (action: UIAlertAction) -> Void in
                Database.clearAll()
                self.reloadData()
            }
            self.promptForConfirmation(title: "Clear", message: "This will delete all groceries and deactivate all recipes. Do you want to continue?", handler: handler)
        }
    }
    
    private func setupAdd() {
        add.showsMenuAsPrimaryAction = true
        add.menu = UIMenu(options: .displayInline, children: [
            UIAction(title: "Add manually", image: UIImage(systemName: "pencil"), handler: { _ in
                self.performSegue(withIdentifier: "addManual", sender: nil)
            }),
            UIAction(title: "Add ingredient photos", image: UIImage(systemName: "photo.on.rectangle"), handler: { _ in
                self.performSegue(withIdentifier: "addPhoto", sender: nil)
            })
        ])
    }
    
    private func setupHamburger() {
        hamburger.showsMenuAsPrimaryAction = true
        hamburger.menu = UIMenu(options: .displayInline, children: [
            UIAction(title: "Share groceries", image: UIImage(systemName: "square.and.arrow.up"), handler: { _ in
                var items: [ReadableGroceryItemWithId] = []
                let groups = Database.selectGroups()
                let groceries = Database.selectGroceries()
                for group in groups {
                    items.append(contentsOf: groceries.filter({
                        $0.item.group?.id == group.id && $0.item.active
                    }))
                }
                items.append(contentsOf: groceries.filter({
                    $0.item.group == nil && $0.item.active
                }))
                if !items.isEmpty {
                    let exportText: String = items.map({ $0.item.render() }).joined(separator: "\n")
                    let vc = UIActivityViewController(activityItems: [exportText], applicationActivities: nil)
                    self.present(vc, animated: true, completion: nil)
                }
            }),
            UIAction(title: "Add group", image: UIImage(systemName: "plus"), handler: { _ in
                self.promptGetInput(title: "Create group", content: nil, configure: { $0.autocapitalizationType = .words }, completion: { (new) in
                    let group = GroceryGroupWithId(group: GroceryGroup(name: new, order: 0), id: UUID())
                    Database.insertGroups(groups: [group])
                    self.reloadData()
                })
            }),
            UIAction(title: "View bought items", image: UIImage(systemName: "dollarsign"), handler: { _ in
                self.performSegue(withIdentifier: "showBought", sender: nil)
            })
        ])
    }

    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddController, segue.identifier == "addManual" {
            vc.addType = .manualGroceries
            vc.onChange = reloadData
        }
        if let vc = segue.destination as? AddController, segue.identifier == "addPhoto" {
            vc.addType = .manualPhoto
            vc.onChange = reloadData
        }
        if let vc = segue.destination as? GroceryListController, segue.identifier == "embedGroceryItems" {
            groceryVc = vc
            vc.count = count
        }
        if let vc = segue.destination as? GroceryListController, segue.identifier == "showBought" {
            vc.active = false
            vc.onChange = reloadData
        }
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        reloadData()
        setupAdd()
        setupHamburger()
    }
}
