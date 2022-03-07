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
    @IBOutlet weak var add: UIButton!

    var groceryVc: GroceryListController? = nil

    @IBAction func didTapClear(_ sender: Any?) {
        let items: [ReadableGroceryItemWithId] = (groceryVc?.toBuy ?? []) + (groceryVc?.bought ?? [])
        if !items.isEmpty {
            let handler = { [weak self] (action: UIAlertAction) -> Void in
                Database.clearAll()
                self?.loadData()
            }
            promptForConfirmation(title: "Clear", message: "Are you sure you want to clear?", handler: handler)
        }
    }

    @IBAction func didTapExport(_ sender: Any) {
        let items: [ReadableGroceryItemWithId] = Database.selectGroceries().filter({ $0.item.active })
        if !items.isEmpty {
            let exportText: String = items.map({ $0.item.render() }).joined(separator: "\n")
            let vc = UIActivityViewController(activityItems: [exportText], applicationActivities: nil)
            present(vc, animated: true, completion: nil)
        }
    }

    func loadData() {
        let groceries = Database.selectGroceries()
        groceryVc?.toBuy = groceries.map({ ReadableGroceryItemWithId(item: $0.item, id: $0.id) }).filter({ $0.item.active })
        groceryVc?.bought = groceries.map({ ReadableGroceryItemWithId(item: $0.item, id: $0.id) }).filter({ !$0.item.active })
        DispatchQueue.main.async {
            self.groceryVc?.tableView.reloadData()
        }
    }

    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddController, segue.identifier == "addGroceries" {
            vc.onChange = loadData
        }
        if let vc = segue.destination as? GroceryListController, segue.identifier == "embedGroceryItems" {
            groceryVc = vc
            loadData()
            vc.onChange = loadData
        }
        if let vc = segue.destination as? BannerController, segue.identifier == "embedBanner" {
            vc.height = banner.constraints.filter({ $0.identifier == "height" }).first
        }
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        clear.frame = CGRect(x: clear.frame.minX, y: clear.frame.minY, width: clear.frame.width, height: toolbar.frame.height)
        clear.alignTextUnderImage()
        export.frame = CGRect(x: export.frame.minX, y: export.frame.minY, width: export.frame.width, height: toolbar.frame.height)
        export.alignTextUnderImage()
        add.frame = CGRect(x: add.frame.minX, y: add.frame.minY, width: add.frame.width, height: toolbar.frame.height)
        add.alignTextUnderImage()
    }

    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        loadData()
    }
}
