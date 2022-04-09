//
//  GroceryController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import GoogleMobileAds
import UIKit

class GroceryController: UIViewController {
    @IBOutlet weak var banner: UIView!
    @IBOutlet weak var toolbar: Toolbar!
    @IBOutlet weak var clear: UIButton!
    @IBOutlet weak var export: UIButton!
    @IBOutlet weak var options: UIButton!
    @IBOutlet weak var add: UIButton!

    var groceryVc: GroceryListController? = nil

    @IBAction func didTapClear(_ sender: Any?) {
        if !Database.selectGroceries().isEmpty {
            let handler = { (action: UIAlertAction) -> Void in
                Database.clearAll()
                self.reloadData()
            }
            promptForConfirmation(title: "Clear", message: "This will delete all groceries and deactivate all recipes. Do you want to continue?", handler: handler)
        }
    }

    @IBAction func didTapExport(_ sender: Any) {
        let items = Database.selectGroceries().filter({ $0.item.active })
        if !items.isEmpty {
            let exportText: String = items.map({ $0.item.render() }).joined(separator: "\n")
            let vc = UIActivityViewController(activityItems: [exportText], applicationActivities: nil)
            present(vc, animated: true, completion: nil)
        }
    }

    func reloadData() {
        groceryVc?.reloadData()
    }
    
    private func setupOptions() {
        options.showsMenuAsPrimaryAction = true
        options.menu = UIMenu(children: [
            UIAction(title: "Add group", image: UIImage(systemName: "plus"), handler: { _ in
                self.promptGetInput(title: "Create group", content: nil, configure: { $0.autocapitalizationType = .words }, completion: { (new) in
                    let group = GroceryGroupWithId(group: GroceryGroup(name: new, order: 0), id: UUID())
                    Database.insertGroups(groups: [group])
                    self.reloadData()
                })
            }),
            UIAction(title: "Expand all groups", image: UIImage(systemName: "arrowtriangle.left.and.line.vertical.and.arrowtriangle.right"), handler: { _ in
                self.groceryVc?.toBuyGroupCollapsed.expandAll()
                self.groceryVc?.boughtGroupCollapsed.expandAll()
                self.reloadData()
            }),
            UIAction(title: "Collapse all groups", image: UIImage(systemName: "arrowtriangle.right.and.line.vertical.and.arrowtriangle.left"), handler: { _ in
                self.groceryVc?.toBuyGroupCollapsed.collapseAll()
                self.groceryVc?.boughtGroupCollapsed.collapseAll()
                self.reloadData()
            }),
            UIAction(title: "Reset groups", image: UIImage(systemName: "arrow.clockwise"), attributes: .destructive, handler: { _ in
                self.promptForConfirmation(title: "Reset groups", message: "This will uncategorize all items. Do you want to continue?", handler: { _ in
                    Database.uncategorizeAll()
                    self.groceryVc?.toBuyGroupCollapsed.collapseAll()
                    self.groceryVc?.boughtGroupCollapsed.collapseAll()
                    self.groceryVc?.toBuyGroupCollapsed.toggleGroupCollapsed(nil)
                    self.groceryVc?.boughtGroupCollapsed.toggleGroupCollapsed(nil)
                    self.reloadData()
                })
            })
        ])
    }

    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddController, segue.identifier == "addGroceries" {
            vc.onChange = reloadData
        }
        if let vc = segue.destination as? GroceryListController, segue.identifier == "embedGroceryItems" {
            groceryVc = vc
        }
        if let vc = segue.destination as? BannerController, segue.identifier == "embedBanner" {
            vc.height = banner.constraints.filter({ $0.identifier == "height" }).first
        }
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        reloadData()
        clear.frame = CGRect(x: clear.frame.minX, y: clear.frame.minY, width: clear.frame.width, height: toolbar.frame.height)
        clear.alignTextUnderImage()
        export.frame = CGRect(x: export.frame.minX, y: export.frame.minY, width: export.frame.width, height: toolbar.frame.height)
        export.alignTextUnderImage()
        options.frame = CGRect(x: options.frame.minX, y: options.frame.minY, width: options.frame.width, height: toolbar.frame.height)
        options.alignTextUnderImage()
        add.frame = CGRect(x: add.frame.minX, y: add.frame.minY, width: add.frame.width, height: toolbar.frame.height)
        add.alignTextUnderImage()
        setupOptions()
    }

    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        reloadData()
    }
}
