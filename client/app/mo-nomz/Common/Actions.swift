//
//  Actions.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/11/21.
//

import UIKit
import Foundation

extension UIViewController {
    func loadUser(completion: ((CreateUserResponse) -> Void)?) {
        let spinner = startLoading()
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: {
                do {
                    let output = try JSONDecoder().decode(CreateUserResponse.self, from: data!)
                    completion?(output)
                } catch {
                    self.defaultOnError(error)
                }
            })
        })
        task.resume()
    }
    
    func loadGroceryItems(completion: ((ListGroceryItemResponse) -> Void)?) {
        let spinner = startLoading()
        guard let state = Persistence.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/grocery")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Accept")
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: {
                do {
                    let output = try JSONDecoder().decode(ListGroceryItemResponse.self, from: data!)
                    completion?(output)
                } catch {
                    self.defaultOnError(error)
                }
            })
        })
        task.resume()
    }
    
    func updateGroceryItem(groceryItemId: Int, groceryItem: ReadableGroceryItem, completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = Persistence.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/grocery")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(UpdateGroceryItemRequest(id: groceryItemId, name: groceryItem.name, quantity: groceryItem.quantity, unit: groceryItem.unit, active: groceryItem.active, order: groceryItem.order))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: completion)
        })
        task.resume()
    }
    
    func mergeGroceryItems(groceryItemIds: [Int], groceryItem: ReadableGroceryItem, completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = Persistence.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/grocery/merge")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(MergeGroceryItemRequest(ids: groceryItemIds, name: groceryItem.name, quantity: groceryItem.quantity, unit: groceryItem.unit, active: groceryItem.active, order: groceryItem.order))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: completion)
        })
        task.resume()
    }
    
    func deleteGroceryItems(groceryItemIds: [Int], completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = Persistence.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/grocery")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "DELETE"
        req.httpBody = try? JSONEncoder().encode(DeleteGroceryItemRequest(ids: groceryItemIds))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: completion)
        })
        task.resume()
    }
    
    func clearGroceryItems(completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = Persistence.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/grocery/clear")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: completion)
        })
        task.resume()
    }
    
    func loadRecipes(completion: ((ListRecipeResponse) -> Void)?) {
        let spinner = startLoading()
        guard let state = Persistence.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/recipe")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Accept")
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: {
                do {
                    let output = try JSONDecoder().decode(ListRecipeResponse.self, from: data!)
                    completion?(output)
                } catch {
                    self.defaultOnError(error)
                }
            })
        })
        task.resume()
    }
    
    func addGroceryList(items: [ImportGrocerySingle], completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = Persistence.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/grocery/list")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ImportGroceryListRequest(items: items))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: completion)
        })
        task.resume()
    }
    
    func addGroceryBlob(name: String?, content: String, active: Bool, completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = Persistence.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/grocery/blob")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ImportGroceryBlobRequest(name: name, content: content, active: active))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onUnsuccessfulStatus = { (resp: URLResponse?) -> Void in self.alertUnsuccessful("Couldn't parse items. Please use one line per item, leading with the quantity.")
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: completion, onUnsuccessfulStatus: onUnsuccessfulStatus, onError: self.defaultOnError)
        })
        task.resume()
    }
    
    func addRecipeLink(link: String, active: Bool, completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = Persistence.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/recipe/link")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ImportRecipeLinkRequest(link: link, active: active))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onUnsuccessfulStatus = { (resp: URLResponse?) -> Void in self.alertUnsuccessful("Unable to import recipe. Please enter ingredients manually.")
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: completion, onUnsuccessfulStatus: onUnsuccessfulStatus, onError: self.defaultOnError)
        })
        task.resume()
    }
    
    func updateRecipe(id: Int, active: Bool, rating: Int, notes: String, completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = Persistence.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/recipe")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(UpdateRecipeRequest(id: id, active: active, rating: rating, notes: notes))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: completion)
        })
        task.resume()
    }
    
    func deleteRecipes(recipeIds: [Int], completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = Persistence.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/recipe")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "DELETE"
        req.httpBody = try? JSONEncoder().encode(DeleteRecipeRequest(ids: recipeIds))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: completion)
        })
        task.resume()
    }
}
