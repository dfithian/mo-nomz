//
//  GroceryController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import UIKit

class GroceryController: UIViewController {
    var groceryVc: GroceryListController? = nil
    
    @IBAction func export(_ sender: Any?) {
        let items: [ReadableGroceryItemAggregate] = groceryVc?.toBuy ?? []
        let exportText: String = items.map({ (x: ReadableGroceryItemAggregate) -> String in
            return x.item.render()
        }).joined(separator: "\n")
        let vc = UIActivityViewController(activityItems: [exportText], applicationActivities: nil)
        present(vc, animated: true, completion: nil)
    }
    
    @IBAction func clear(_ sender: Any?) {
        let handler = { [weak self] (action: UIAlertAction) -> Void in self?.clearGroceryItems(completion: self?.loadGroceryItems) }
        promptForConfirmation(title: "Clear", message: "Are you sure you want to clear your grocery list?", handler: handler)
    }
    
    private func loadGroceryItems() {
        let completion = { [weak self] (resp: ListGroceryItemResponse) -> Void in
            let items = resp.items
            self?.groceryVc?.toBuy = items.filter({ $0.item.active })
            self?.groceryVc?.bought = items.filter({ !$0.item.active })
            DispatchQueue.main.async {
                self?.groceryVc?.tableView.reloadData()
            }
        }
        loadGroceryItems(completion: completion)
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? RecipeAddController, segue.identifier == "addLink" {
            vc.onChange = { () -> Void in
                self.loadGroceryItems()
            }
        }
        if let vc = segue.destination as? GroceryAddBlobController, segue.identifier == "addBlob" {
            vc.onChange = { () -> Void in
                self.loadGroceryItems()
            }
        }
        if let vc = segue.destination as? GroceryAddController, segue.identifier == "addGroceries" {
            vc.onChange = { () -> Void in
                self.loadGroceryItems()
            }
        }
        if let vc = segue.destination as? GroceryListController, segue.identifier == "embedGroceryItems" {
            groceryVc = vc
            loadGroceryItems()
            vc.onChange = { () -> Void in
                self.loadGroceryItems()
            }
        }
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        loadGroceryItems()
    }
}
