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
    
    @IBAction func export(_ sender: Any?) {
        let items: [ReadableGroceryItemWithId] = groceryVc?.toBuy ?? []
        if !items.isEmpty {
            let itemsText: String = items.map({ (x: ReadableGroceryItemWithId) -> String in
                return x.item.render()
            }).joined(separator: "\n")
            let exportText: String = "Grocery List\n\(itemsText)"
            let vc = UIActivityViewController(activityItems: [exportText], applicationActivities: nil)
            present(vc, animated: true, completion: nil)
        }
    }
    
    @IBAction func clear(_ sender: Any?) {
        let items: [ReadableGroceryItemWithId] = (groceryVc?.toBuy ?? []) + (groceryVc?.bought ?? [])
        if !items.isEmpty {
            let handler = { [weak self] (action: UIAlertAction) -> Void in
                self?.clearAll()
                self?.loadData()
            }
            promptForConfirmation(title: "Clear", message: "Are you sure you want to clear?", handler: handler)
        }
    }
    
    @objc func loadData() {
        let groceries = selectGroceries()
        groceryVc?.toBuy = groceries.map({ ReadableGroceryItemWithId(item: $0.item, id: $0.id) }).filter({ $0.item.active }).sorted(by: { $0.item.order < $1.item.order })
        groceryVc?.bought = groceries.map({ ReadableGroceryItemWithId(item: $0.item, id: $0.id) }).filter({ !$0.item.active }).sorted(by: { $0.item.order < $1.item.order })
        DispatchQueue.main.async {
            self.groceryVc?.tableView.reloadData()
        }
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? GroceryAddController, segue.identifier == "addGroceries" {
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
        NotificationCenter.default.addObserver(self, selector: #selector(loadData), name: UIApplication.willEnterForegroundNotification, object: nil)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        loadData()
    }
}
