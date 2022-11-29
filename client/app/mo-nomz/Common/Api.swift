//
//  Api.swift
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
            let onError = {
                self.alertUnsuccessful("Failed to initialize. Please close the app and try again.")
            }
            let onSuccess = { (data: Data) -> Void in
                do {
                    let output = try JSONDecoder().decode(CreateUserResponse.self, from: data)
                    completion?(output)
                } catch {
                    onError()
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onError: onError)
        })
        task.resume()
    }
    
    func pingUser(completion: (() -> Void)?) {
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/ping")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        let version = Bundle.main.infoDictionary?["CFBundleShortVersionString"] as! String
        let bundle = Bundle.main.infoDictionary?["CFBundleVersion"] as! String
        #if targetEnvironment(simulator)
        let target = "simulator"
        #else
        let target = "device"
        #endif
        req.httpBody = try? JSONEncoder().encode(UserPingRequest(version: "\(version).\(bundle)", target: target))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            completion?()
        })
        task.resume()
    }
    
    func initializeGroups(completion: (() -> Void)?) {
        let rawGroups = [
            "Produce",
            "Meat/Seafood",
            "Dairy/Eggs",
            "Dry/Canned",
            "Frozen",
            "Household"
        ]
        var groups: [GroceryGroupWithId] = []
        var order = 0
        for rawGroup in rawGroups {
            groups.append(GroceryGroupWithId(group: GroceryGroup(name: rawGroup, order: order), id: UUID()))
            order += 1
        }
        Database.insertGroups(groups: groups)
        User.setDidInitializeGroups()
        completion?()
    }

    func addLink(link: String, completion: ((ParseLinkResponse) -> Void)?) {
        let spinner = startLoading()
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/link")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ParseLinkRequest(link: link))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onError = { self.alertUnsuccessful("Unable to import recipe. Please select photos or enter ingredients manually.")
            }
            let onSuccess = { (d: Data) -> Void in
                do {
                    let output = try JSONDecoder().decode(ParseLinkResponse.self, from: d)
                    completion?(output)
                } catch {
                    onError()
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onError: onError)
        })
        task.resume()
    }

    func addBlob(content: String, completion: (([ReadableIngredient]) -> Void)?) {
        let spinner = startLoading()
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/blob")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ParseBlobRequest(content: content))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onError = {
                let rawIngredients = content.components(separatedBy: "\n").compactMap({ $0.nonEmpty() })
                var ingredients = [ReadableIngredient]()
                var order = 0
                for ingredient in rawIngredients {
                    ingredients.append(ReadableIngredient(name: ingredient, quantity: ReadableQuantity(whole: nil, fraction: nil), unit: nil, order: order))
                    order += 1
                }
                Database.insertGroceries(ingredients: ingredients)
                completion?(ingredients)
            }
            let onSuccess = { (d: Data) -> Void in
                do {
                    let output = try JSONDecoder().decode(ParseBlobResponse.self, from: d)
                    Database.insertGroceries(ingredients: output.ingredients)
                    completion?(output.ingredients)
                } catch {
                    onError()
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onError: onError)
        })
        task.resume()
    }
    
    func addBlob(content: String, name: String, link: String?, rawSteps: [String], active: Bool, completion: ((ReadableRecipeWithId) -> Void)?) {
        let spinner = startLoading()
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/blob")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ParseBlobRequest(content: content))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onError = {
                let rawIngredients = content.components(separatedBy: "\n").compactMap({ $0.nonEmpty() })
                var ingredients = [ReadableIngredient]()
                var order = 0
                for ingredient in rawIngredients {
                    ingredients.append(ReadableIngredient(name: ingredient, quantity: ReadableQuantity(whole: nil, fraction: nil), unit: nil, order: order))
                    order += 1
                }
                let recipe = Database.insertRecipe(response: ParseBlobResponse(ingredients: ingredients), name: name, link: link, rawSteps: rawSteps, active: active, tags: [])
                completion?(recipe)
            }
            let onSuccess = { (d: Data) -> Void in
                do {
                    let output = try JSONDecoder().decode(ParseBlobResponse.self, from: d)
                    let recipe = Database.insertRecipe(response: output, name: name, link: link, rawSteps: rawSteps, active: active, tags: [])
                    completion?(recipe)
                } catch {
                    
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onError: onError)
        })
        task.resume()
    }
}
