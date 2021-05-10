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
        let handler = { [weak self] (action: UIAlertAction) -> Void in self?.clearGroceryItems(completion: self?.loadData) }
        promptForConfirmation(title: "Clear", message: "Are you sure you want to clear your grocery list?", handler: handler)
    }
    
    @objc func loadData() {
        let completion = { [weak self] (resp: ListGroceryItemResponse) -> Void in
            let items = resp.items
            self?.groceryVc?.toBuy = items.map({ ReadableGroceryItemWithId(item: $0.value, id: $0.key) }).filter({ $0.item.active }).sorted(by: { $0.item.order < $1.item.order })
            self?.groceryVc?.bought = items.map({ ReadableGroceryItemWithId(item: $0.value, id: $0.key) }).filter({ !$0.item.active }).sorted(by: { $0.item.order < $1.item.order })
            DispatchQueue.main.async {
                self?.groceryVc?.tableView.reloadData()
            }
        }
        loadGroceryItems(completion: completion)
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? RecipeAddController, segue.identifier == "addLink" {
            vc.onChange = { () -> Void in
                self.loadData()
            }
        }
        if let vc = segue.destination as? GroceryAddController, segue.identifier == "addGroceries" {
            vc.onChange = { () -> Void in
                self.loadData()
            }
        }
        if let vc = segue.destination as? GroceryListController, segue.identifier == "embedGroceryItems" {
            groceryVc = vc
            loadData()
            vc.onChange = { () -> Void in
                self.loadData()
            }
        }
        if let vc = segue.destination as? BannerController, segue.identifier == "embedBanner" {
            vc.height = banner.constraints.filter({ $0.identifier == "height" }).first
        }
    }
    
    override func viewDidLoad() {
        NotificationCenter.default.addObserver(self, selector: #selector(loadData), name: UIApplication.willEnterForegroundNotification, object: nil)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        loadData()
    }
}
